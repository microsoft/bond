#!/usr/bin/env python3

"""Main entry point for garbage image cleanup."""

import argparse
import logging
import subprocess
import sys

from datetime import timedelta
from typing import Iterable

from collector.acr import delete_image_by_manifest, get_image_manifests
from collector.garbage_manifests import find_garbage_manifests
from collector.live_images import live_tags

PROGRAM_DESCRIPTION = """Remove images from an Azure Container Registry repository if they are no
longer needed. Images are considered needed a) if they are referenced by the
.travis.yml or Linux build GitHub Action workflow files in any of the
commits included by the provided root_filters, or b) if they are younger
than the provided min-age."""

def semi_list(semi_str: str) -> Iterable[str]:
    """Splits a semi-colon delimited string into a list of string."""
    return semi_str.split(';')

def main() -> None:
    """Program main entry point."""
    parser = argparse.ArgumentParser(description=PROGRAM_DESCRIPTION)
    parser.add_argument('--repo-path', '-p',
                        required=True,
                        help='path to the repository to inspect')
    parser.add_argument('root_filters',
                        nargs='+',
                        type=semi_list,
                        help="""semi-colon seperated list of arguments for one invocation of `git
                        rev-parse`. The commits in the union of these
                        rev-parse invocations are used to find container images
                        that are referenced by source code. Ex:
                        '--remotes=origin;--since=2~weeks~ago' or '--tags;-n;1'""")
    parser.add_argument('--min-age', '-m',
                        required=True,
                        type=int,
                        help="""the minimum length of time, in days, before images are eligible for
                        deletion, even if they are not referenced by the source code.""")
    parser.add_argument('--dry-run', '-n',
                        action='store_true',
                        help="""report which images would be deleted, but do not actually delete
                        them""")
    parser.add_argument('--verbosity', '-v',
                        default='WARNING',
                        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL', 'NOTSET'],
                        help='log level to emit to stderr. Defaults to WARNING.')
    args = parser.parse_args()

    assert args.root_filters

    numeric_level = getattr(logging, args.verbosity.upper(), None)
    if not isinstance(numeric_level, int):
        raise ValueError('Invalid --verbosity level: {}'.format(args.verbosity))

    logging.basicConfig(level=numeric_level)

    if args.min_age < 0:
        raise ValueError('--min-age must be non-negative, but got {}'.format(args.min_age))

    min_age_before_gc = timedelta(days=args.min_age)

    try:
        active_tags = live_tags(args.repo_path, args.root_filters)
        if not active_tags:
            raise ValueError('No active tags. This can delete all images, so aborting.')
        logging.info('Active tags: {%s}', ','.join(active_tags))

        manifests = get_image_manifests()

        for manifest in find_garbage_manifests(min_age_before_gc, active_tags, manifests):
            if args.dry_run:
                print('{}: would delete'.format(manifest.digest))
            else:
                delete_image_by_manifest(manifest)
                print('{}: deleted'.format(manifest.digest))

    except subprocess.CalledProcessError as cpe:
        print('Subprocess {} failed with exit code {}'.format(
            cpe.cmd,
            cpe.returncode,
            file=sys.stderr))
        print('STDOUT:\n', str(cpe.stdout, encoding='utf-8'), file=sys.stderr)
        print('STDERR:\n', str(cpe.stderr, encoding='utf-8'), file=sys.stderr)
        raise

if __name__ == '__main__':
    main()
