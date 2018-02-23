"""Functions to determine whether ACR images are garbage."""

import logging

from datetime import datetime, timedelta, timezone
from typing import AbstractSet, Iterable

from .acr import ImageManifest
from .live_images import ImageTag

_LOGGER = logging.getLogger(__name__)

def find_garbage_manifests(
        min_age: timedelta,
        active_tags: AbstractSet[ImageTag],
        image_manifests: Iterable[ImageManifest]) -> Iterable[ImageManifest]:
    """Yield a sequence of the manifests that are considered garbage.

    Image manifests are considered garbage if they are both:
    * older than `min_age` and
    * not tagged with a tag in `active_tags`.

    Typically `active_tags` is computed by calling `live_images.live_tags`.
    """
    keep_newer_than_time = datetime.now(timezone.utc) - min_age
    _LOGGER.debug('Keeping images newer than %s', keep_newer_than_time)

    for manifest in image_manifests:
        # It would probably be faster to check timestamps before tags, but
        # the tags are more important of a reason to keep an image, so we
        # check tags first so that the tag reference gets logged instead of
        # the newness.
        matching_tags = manifest.tags.intersection(active_tags)
        if matching_tags:
            _LOGGER.info('%s: keep - referenced {%s}', manifest.digest, ','.join(matching_tags))
            continue

        if manifest.timestamp > keep_newer_than_time:
            _LOGGER.info('%s: keep - too new (%s)', manifest.digest, manifest.timestamp)
            continue

        _LOGGER.info('%s: garbage', manifest.digest)
        yield manifest
