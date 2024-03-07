##############################################################################
# (c) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
# Makes driver routine based on psyad files.
PSYAD_FILES := $(shell cat $(ADJOINT_BUILD)/psyad_files_list.txt)
PSYAD_FILES := $(addprefix $(ROOT_DIR)/,$(PSYAD_FILES))

DRIVER_TEMPLATE_PATH := $(ADJOINT_BUILD)/gen_adj_kernel_tests_mod.txt
DRIVER_TARGET := $(WORKING_DIR)/driver/gen_adj_kernel_tests_mod.f90
DIRECTORIES := $(WORKING_DIR)/driver

all: $(DRIVER_TARGET)

$(DRIVER_TARGET): | $(DIRECTORIES)
	cp $(DRIVER_TEMPLATE_PATH) $@
	python $(ADJOINT_BUILD)/psyad_driver.py -f $(PSYAD_FILES) -d $@

$(DIRECTORIES):
	mkdir -p $@

