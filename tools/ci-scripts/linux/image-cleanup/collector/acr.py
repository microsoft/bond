"""Functions and types for interacting with Azure Container Registry via the
``az`` command line tool."""

import json
import logging
import subprocess

from datetime import datetime, timezone
from typing import Mapping, Iterable, Generator, NamedTuple

_LOGGER = logging.getLogger(__name__)

class ManifestParseError(Exception):
    """Represents an error parsing a manifest dictionary.

    Used to collect the dictionary that was parsed to ease debugging.
    """

    def __init__(self, dct: Mapping[str, str]) -> None:
        super().__init__()
        self.dct = dct

    def __str__(self) -> str:
        return str(self.dct)

class ImageManifest: # pylint: disable=too-few-public-methods
    """Represents an ACR image manifest."""

    @staticmethod
    def _parse_manifest_timestamp(timestamp: str) -> datetime:
        if not timestamp.endswith('Z'):
            msg = 'Can only parse UTC timestamps (that end with Z), but go {}'.format(timestamp)
            raise ValueError(msg)

        # ACR timestamps sometimes have fractional seconds. There's no RFC
        # 3339 or ISO 8601 parser built in to Python, but we don't need this
        # much precision, so we just strip the fractional seconds.
        dot_loc = timestamp.rfind('.')
        if dot_loc != -1:
            timestamp = timestamp[0:dot_loc] + 'Z'

        return datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=timezone.utc)

    def __init__(self, **kwargs: str) -> None:
        """Initalize an ImageManifest from a dictionary.

        This is written to handle the JSON representation of one
        manifest from the command ``az acr repository list-manifests``
        (which returns a list).

        Each manifest looks like:::
            {
                "digest": "sha256:1884e693b47726118459af31699a816fcd103a3567397d4306e5b435f64dec33",
                "tags": [
                    "build-9867746",
                    "git-da33ddf6b9c24c14bb7d836adff8ac77f523d047"
                ],
                "timestamp": "2018-03-22T17:32:05.3564254Z"
            }
        """

        try:
            _LOGGER.debug('Parsing %s', kwargs)

            self.digest = kwargs['digest']
            self.tags = frozenset(kwargs['tags'])
            self.timestamp = ImageManifest._parse_manifest_timestamp(kwargs['timestamp'])
        except:
            raise ManifestParseError(kwargs)

class NamedImageManifest(NamedTuple):
    """Represents an ACR image manifest with a human readeable name."""
    manifest: ImageManifest
    registry_name: str
    repository_name: str

def get_image_manifests(registry_name: str, repository_names: Iterable[str]) -> Generator[NamedImageManifest, None, None]:
    """Get the current ACR image manifests.

    Invokes the ``az`` CLI tool to discover the current images.
    """
    for repository_name in repository_names:
        az_show_manifests_cmd_line = ['az', 'acr', 'repository', 'show-manifests',
                                    '--name', registry_name,
                                    '--repository', repository_name,
                                    '--output', 'json']
        _LOGGER.debug('Invoking %s', az_show_manifests_cmd_line)
        output = subprocess.check_output(az_show_manifests_cmd_line,
                                        stderr=subprocess.PIPE)
        manifests = json.loads(str(output, encoding='utf-8'))

        if not isinstance(manifests, list):
            msg = 'Expected an array of manifests ("[{{...}},{{...}}]" but got {}'.format(
                type(manifests).__name__)
            raise ValueError(msg)
        
        for o in manifests:
            yield NamedImageManifest(ImageManifest(**o), registry_name, repository_name)

def delete_image_by_manifest(named_manifest: NamedImageManifest) -> None:
    """Delete an ACR image (and all its tags)."""
    image_name = '{}@{}'.format(named_manifest.repository_name, named_manifest.manifest.digest)
    az_delete_cmd_line = ['az', 'acr', 'repository', 'delete',
                          '--name', named_manifest.registry_name,
                          '--image', image_name,
                          '--yes']
    _LOGGER.debug('Invoking %s', az_delete_cmd_line)
    p = subprocess.run(az_delete_cmd_line, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p.check_returncode()
