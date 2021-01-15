"""Find referenced images from Git commits."""

import logging
import re
import subprocess

from typing import AbstractSet, Iterable, NewType, Sequence, Set # pylint: disable=unused-import

from .config import REGISTRY_NAME, REPOSITORY_NAME

_LOGGER = logging.getLogger(__name__)

_BlobId = NewType('_BlobId', str) # pylint: disable=invalid-name

"""A type for a collection of parameters to ``git rev-list``."""
RevListRoots = NewType('RevListRoots', Sequence[Sequence[str]]) # pylint: disable=invalid-name

"""A type for the full name of an image. E.g.,
"bondciimages.azurecr.io/ubuntu-1604:build-12345".
"""
ImageName = NewType('ImageName', str) # pylint: disable=invalid-name

"""A type for the an image tag. E.g., "build-12345"."""
ImageTag = NewType('ImageTag', str) # pylint: disable=invalid-name

def _blobs_from_roots(
        repo_path: str,
        roots: RevListRoots) -> AbstractSet[_BlobId]:
    """Return the blob IDs of the revisions of .travis.yml and Linux build
GitHub Actions for all the commits specified by the given `roots`.

    :param repo_path: Path to the repository to inspect.

    :param roots: A collection of argument lists to pass to ``git
    rev-list`` to limit the matched commits.

    :return The blob IDs of the workflow file revisions.
    """

    commit_ids = set()
    for root_args in roots:
        git_rev_list_cmd_line = ['git', '-C', repo_path, 'rev-list']
        git_rev_list_cmd_line.extend(root_args)

        _LOGGER.debug('Invoking %s', git_rev_list_cmd_line)

        rev_list_output = subprocess.check_output(git_rev_list_cmd_line, stderr=subprocess.PIPE)
        commit_ids.update(str(rev_list_output, 'utf-8').splitlines())
        _LOGGER.debug('Commit IDs currently: %s', commit_ids)

    blob_ids = set()
    for commit in commit_ids:
        # ls-tree takes paths, not pathspecs, so globbing can't be used to
        # match multiple files in the same directory.
        #
        # If the set files grows much larger than what we have here, they
        # should be taken from the command line or a config file.
        git_ls_tree_cmd_line = ['git',
                                '-C', repo_path,
                                'ls-tree', commit,
                                '--',
                                ':/.travis.yml',
                                ':/.github/workflows/linux.yml',
                                ':/.github/workflows/linux_cron.yml']
        _LOGGER.debug('Invoking %s', git_ls_tree_cmd_line)
        ls_tree_output = subprocess.check_output(git_ls_tree_cmd_line, stderr=subprocess.PIPE)

        for line in str(ls_tree_output, 'utf-8').splitlines():
            _LOGGER.debug('Parsing ls-tree output: "%s"', line)

            # Expecing output like:
            #
            # 100644 blob d68588e35c6411d545deabd40d9c987c11dc4748	.github/workflows/linux.yml
            # 100644 blob 614fec24bedc561418a9b1df54674d4b7a04396b	.github/workflows/linux_cron.yml
            # 100644 blob 5f425a18a84a66d137cffc9db77b3c3eb3e20282	.travis.yml
            parts = line.split()

            # The length of parts may not be exactly 4 if the name
            # contains a space. (Though, this shouldn't happen in this
            # particular case.) Instead of parsing exactly, we check
            # that we have at least the first three parts and call it
            # good enough.
            if len(parts) < 3:
                raise ValueError('Cannot parse ls-tree output: "{}"'.format(line))

            obj_type = parts[1]
            blob_id = _BlobId(parts[2])
            if obj_type == 'blob':
                blob_ids.add(blob_id)

    _LOGGER.debug('Blobs: %s', blob_ids)
    return frozenset(blob_ids)

def _images_from_blobs(
        repo_path: str,
        commits: Iterable[_BlobId]) -> AbstractSet[ImageName]:
    """Get the full names (including tags) of the images used for the
CI build in the given blobs.

    :param repo_path: Path to the repository to inspect.

    :param commits: The Git blob IDs of files in which to look for image
    names. It's assumed that these are blob IDs of .travis.yml, GitHub
    Action workflow YAML, or similar files.

    :return The full image names used by the specificed blobs.

    Images are found by looking for a lines like

    Travis CI: CI_BUILD_IMAGE=...
    GitHub Actions: image: ...
    """

    image_names = set() # type: Set[ImageName]

    for commit in commits:
        git_show_cmd_line = ['git', '-C', repo_path, 'show', commit]
        _LOGGER.debug('Invoking %s', git_show_cmd_line)
        git_show_output = subprocess.check_output(git_show_cmd_line, stderr=subprocess.PIPE)

        matches = re.finditer(b'^.+(CI_BUILD_IMAGE=|image:)(.+)\\s*$',
                              git_show_output,
                              re.MULTILINE)
        matched_names = (ImageName(str(match.group(2), 'utf-8').strip())
                         for match in matches)
        image_names.update(matched_names)
        _LOGGER.debug('Image names currently: %s', image_names)

    return frozenset(image_names)

def live_images(
        repo_path: str,
        roots: RevListRoots) -> AbstractSet[ImageName]:
    """Return the image names used by all of the commits specified by the
given `roots`.

    :param repo_path: Path to the repository to inspect.

    :param roots: A collection of argument lists to pass to ``git
    rev-list`` to limit the matched commits.
    """
    return _images_from_blobs(repo_path,
                              _blobs_from_roots(repo_path, roots))

def live_tags(repo_path: str,
              roots: RevListRoots) -> AbstractSet[ImageTag]:
    """Return the image tags that are referenced by .travis.yml or Linux GitHub
Action workflow files in the commits specified by the given `roots`.

    :param repo_path: Path to the repository to inspect.

    :param roots: A collection of argument lists to pass to ``git rev-list``
    to limit the matched commits.
    """
    expected_prefix = '{}.azurecr.io/{}'.format(REGISTRY_NAME, REPOSITORY_NAME)
    def matches_expected_prefix(image_name: ImageName) -> bool:
        """Check (and log) whether an image name is from the expected repository."""
        if image_name.startswith(expected_prefix):
            return True

        _LOGGER.info(
            'Discarding image "%s" that does not match expected prefix "%s"',
            image_name,
            expected_prefix)
        return False

    return frozenset((ImageTag(image_name.split(':')[1])
                      for image_name in live_images(repo_path, roots)
                      if matches_expected_prefix(image_name)))
