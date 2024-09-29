# Directories
SRC_DIR = src
GEN_DIR = generated
CODEGEN_DIR = code_gen

# Code generation input files
SCRIPT_NAMES_INPUT = pcre2test/pcre2test-LS.txt
BINARY_PROPERTIES_INPUT = pcre2test/pcre2test-LP.txt

# Code generation Haskell script
AUTO_GEN_SCRIPT = $(CODEGEN_DIR)/code_gen.hs

# Output files
SCRIPT_NAMES_OUTPUTS = $(GEN_DIR)/AbsScriptName.hs $(GEN_DIR)/ParseHelpScriptName.hs
BINARY_PROPERTIES_OUTPUTS = $(GEN_DIR)/AbsBinProp.hs $(GEN_DIR)/ParseHelpBinProp.hs
GENERATED_FILES = $(SCRIPT_NAMES_OUTPUTS) $(BINARY_PROPERTIES_OUTPUTS)

all: generate_files build

generate_files: $(GENERATED_FILES)

$(GENERATED_FILES): $(SCRIPT_NAMES_INPUT) $(BINARY_PROPERTIES_INPUT) $(AUTO_GEN_SCRIPT)
	mkdir -p $(GEN_DIR)
	stack runhaskell $(AUTO_GEN_SCRIPT)

build: generate_files
	stack build

install: generate_files build
	stack install

clean:
	rm -f $(GENERATED_FILES)
	stack clean

.PHONY: all clean generate_files build install
