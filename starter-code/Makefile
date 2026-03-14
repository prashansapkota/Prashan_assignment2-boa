# Makefile for Boa Compiler

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
NASM_FORMAT := elf64
RUSTC := rustup run stable rustc
RUST_TARGET_FLAG :=
RUN_PREFIX :=
ifeq ($(UNAME_S),Darwin)
NASM_FORMAT := macho64
ifeq ($(UNAME_M),arm64)
RUST_TARGET_FLAG := --target x86_64-apple-darwin
RUN_PREFIX := arch -x86_64
endif
endif

# Pattern rule to compile .snek files to .s assembly files
test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

# Pattern rule to assemble .s files and link into executables
test/%.run: test/%.s runtime/start.rs
	nasm -f $(NASM_FORMAT) test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	$(RUSTC) $(RUST_TARGET_FLAG) -L runtime/ runtime/start.rs -o test/$*.run

# Clean build artifacts
clean:
	cargo clean
	rm -f test/*.s test/*.run runtime/*.o runtime/*.a

# Run all tests
test: test/simple.run test/add.run test/binop.run test/let_simple.run test/let_multi.run test/nested.run
	@echo "Running tests..."
	@echo -n "simple (expected 42): " && $(RUN_PREFIX) ./test/simple.run
	@echo -n "add (expected 6): " && $(RUN_PREFIX) ./test/add.run
	@echo -n "binop (expected 9): " && $(RUN_PREFIX) ./test/binop.run
	@echo -n "let_simple (expected 10): " && $(RUN_PREFIX) ./test/let_simple.run
	@echo -n "let_multi (expected 11): " && $(RUN_PREFIX) ./test/let_multi.run
	@echo -n "nested (expected 25): " && $(RUN_PREFIX) ./test/nested.run

.PHONY: clean test
