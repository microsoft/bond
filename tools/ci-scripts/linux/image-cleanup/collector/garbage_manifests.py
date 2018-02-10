"""Functions to determine whether ACR images are garbage."""

from datetime import datetime, timedelta, timezone
import logging
from typing import AbstractSet, Iterable

from .acr import ImageManifest
from .live_images import ImageTag

logger = logging.getLogger(__name__)

"""Time used as "now" for checking image age."""
CURRENT_TIME = datetime.now(timezone.utc)

def find_garbage_manifests(
        min_age: timedelta,
        active_tags: AbstractSet[ImageTag],
        image_manifests: Iterable[ImageManifest]) -> Iterable[ImageManifest]:
    """Return an iterable of the manifests that are considered garbage.

    Image manifests are considered garbage if they are both:
    * older than `min_age` and
    * not tagged with a tag in `active_tags`.

    Typically `active_tags` is computed by calling `live_images.live_tags`.
    """

    keep_newer_than_time = CURRENT_TIME - min_age

    for manifest in image_manifests:
        # It would probably be faster to check timestamps before tags, but
        # the tags are more important of a reason to keep an image, so we
        # check tags first so that the tag reference gets logged instead of
        # the newness.
        matching_tags = manifest.tags.intersection(active_tags)
        if matching_tags:
            logger.info('%s: keep - referenced {%s}', manifest.digest, ','.join(matching_tags))
            continue

        if manifest.timestamp > keep_newer_than_time:
            logger.info('%s: keep - too new (%s)', manifest.digest, manifest.timestamp)
            continue

        logger.info('%s: garbage', manifest.digest)
        yield manifest
