##############################################################################
# (c) Crown copyright 2020 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""Extract metadata from a Rose suite and an immutable metadata JSON file and
    output it in an XIOS iodef.xml file"""

import argparse
import sys
from pathlib import Path

# append infrastructure/build/tools to path to enable absolute imports
sys.path.append(str(Path(__file__).resolve().parents[2]))

from diagnostics_metadata_reconfigurator.src.metadata_extractor import MetadataExtractor
from diagnostics_metadata_reconfigurator.src.metadata_iodef_generator import MetadataIodefGenerator


def get_args():
    """Get command line arguments

    :return: List of values given from arguments
    """
    parser = argparse.ArgumentParser(
        description="Reconfigure metadata from a rose suite and a JSON "
                    "immutable metadata file into an XIOS iodef.xml file")
    parser.add_argument('rose_suite_path', type=str,
                        help="Path to a Rose suite")
    parser.add_argument('immutable_data_path', type=str,
                        help="Path to JSON file containing immutable metadata")
    parser.add_argument('output_path', type=str,
                        help="Path to output the iodef.xml file to")
    parser.add_argument('--iodef-template', dest='template_path',
                        help="Path to a template iodef.xml file")
    return parser.parse_args()


def reconfigure_metadata():
    """Extract metadata from a Rose suite and an immutable metadata JSON file
        and output it in an XIOS iodef.xml file"""
    args = get_args()
    meta_extractor = MetadataExtractor(args.rose_suite_path,
                                       args.immutable_data_path)
    metadata = meta_extractor.extract_metadata()
    # metadata_validator(metadata) #todo: write a validator in a later phase
    generator = MetadataIodefGenerator(metadata)
    generator.generate(args.output_path, args.template_path)


if __name__ == '__main__':
    reconfigure_metadata()
