# Directories
SRC_DIR = src
GEN_DIR = generated
CODEGEN_DIR = code_gen

# Files
SCRIPT_NAMES_INPUT = pcre2test/pcre2test-LS.txt
BINARY_PROPERTIES_INPUT = pcre2test/pcre2test-LP.txt
AUTO_GEN_SCRIPT = $(CODEGEN_DIR)/code_gen.hs

# Output files
SCRIPT_NAMES_OUTPUTS = $(GEN_DIR)/AbsScriptName.hs $(GEN_DIR)/ParseHelpScriptName.hs
BINARY_PROPERTIES_OUTPUTS = $(GEN_DIR)/AbsBinProp.hs $(GEN_DIR)/ParseHelpBinProp.hs
GENERATED_FILES = $(SCRIPT_NAMES_OUTPUTS) $(BINARY_PROPERTIES_OUTPUTS)

# Default target
all: generate_files build

# Phony target to generate files
generate_files: $(GENERATED_FILES)

# Rule to generate AbsBinProp.hs, ParsHelpBinProp.hs, AbsScriptName.hs and ParsHelpScriptName.hs
$(GENERATED_FILES): $(SCRIPT_NAMES_INPUT) $(BINARY_PROPERTIES_INPUT) $(AUTO_GEN_SCRIPT)
	mkdir -p $(GEN_DIR)
	stack runhaskell $(AUTO_GEN_SCRIPT)

build: generate_files
	stack build

install: generate_files build
	stack install

# Clean generated files
clean:
	rm -f $(GENERATED_FILES)
	stack clean

# Phony targets
.PHONY: all clean generate_files build install
