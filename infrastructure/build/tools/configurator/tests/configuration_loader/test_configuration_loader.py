#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2022 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
import io
from pathlib import Path

import configurator.configurationloader as loader

HERE = Path(__file__).resolve().parent


class TestLoader():
    def test_empty(self, tmp_path: Path):
        output_file = tmp_path / 'empty_mod.f90'
        uut = loader.ConfigurationLoader('empty_mod')
        uut.write_module(output_file)

        expected_file = HERE / 'empty_mod.f90'
        assert output_file.read_text() + '\n' == expected_file.read_text()

    def test_with_content(self, tmp_path: Path):
        uut = loader.ConfigurationLoader('content_mod')
        uut.add_namelist('foo')
        output_file = tmp_path / 'content_mod.f90'
        uut.write_module(output_file)

        expected_file = HERE / 'content_mod.f90'
        assert output_file.read_text() + '\n' == expected_file.read_text()
