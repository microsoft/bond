"""Functions and types for interacting with Azure Container Registry via the
``az`` command line tool."""

import json
import logging
import subprocess

from datetime import datetime, timezone
from typing import Mapping, Iterable

from .config import REGISTRY_NAME, REPOSITORY_NAME

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
                "timestamp": "2018-02-09T23:44:14Z"
            }
        """

        try:
            _LOGGER.debug('Parsing %s', kwargs)

            self.digest = kwargs['digest']
            self.tags = frozenset(kwargs['tags'])
            self.timestamp = datetime.strptime(
                kwargs['timestamp'],
                '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=timezone.utc)
        except:
            raise ManifestParseError(kwargs)

def get_image_manifests() -> Iterable[ImageManifest]:
    """Get the current ACR image manifests.

    Invokes the ``az`` CLI tool to discover the current images.
    """
    az_show_manifests_cmd_line = ['az', 'acr', 'repository', 'show-manifests',
                                  '--name', REGISTRY_NAME,
                                  '--repository', REPOSITORY_NAME,
                                  '--output', 'json']
    _LOGGER.debug('Invoking %s', az_show_manifests_cmd_line)
    output = subprocess.check_output(az_show_manifests_cmd_line,
                                     stderr=subprocess.PIPE)
    manifests = json.loads(str(output, encoding='utf-8'))

    if not isinstance(manifests, list):
        msg = 'Expected an array of manifests ("[{{...}},{{...}}]" but got {}'.format(
            type(manifests).__name__)
        raise ValueError(msg)

    return [ImageManifest(**o) for o in manifests]

def delete_image_by_manifest(manifest: ImageManifest) -> None:
    """Delete a ACR image (and all its tags)."""
    az_delete_cmd_line = ['az', 'acr', 'repository', 'delete',
                          '--name', REGISTRY_NAME,
                          '--repository', REPOSITORY_NAME,
                          '--manifest', manifest.digest,
                          '--yes']
    _LOGGER.debug('Invoking %s', az_delete_cmd_line)
    p = subprocess.run(az_delete_cmd_line, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    p.check_returncode()
