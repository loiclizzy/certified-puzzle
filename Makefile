ROCQ ?= rocq
DUNE ?= dune
PYTHON ?= python3

ROCQ_DIR := rocq
EXTRACTED_ML := src/calendar_verified.ml
JS_BUILD := _build/default/src/verifier_bridge.bc.js
WEB_VERIFIER := web/ocaml_verifier.js

.PHONY: all certified proof extract web preview serve clean distclean check-tools

all: certified

check-tools:
	@command -v $(ROCQ) >/dev/null || { echo "Missing Rocq command: $(ROCQ)"; exit 1; }
	@command -v $(DUNE) >/dev/null || { echo "Missing dune"; exit 1; }

proof: check-tools
	cd $(ROCQ_DIR) && $(ROCQ) compile CalendarPuzzle.v
	cd $(ROCQ_DIR) && $(ROCQ) compile DateEnumeration.v
	cd $(ROCQ_DIR) && $(ROCQ) compile GeneratedSolutions.v
	cd $(ROCQ_DIR) && $(ROCQ) compile AllDatesProof.v

extract: proof
	cd $(ROCQ_DIR) && $(ROCQ) compile Extract.v
	@test -f $(EXTRACTED_ML) || { echo "Extraction did not create $(EXTRACTED_ML)"; exit 1; }

web: extract
	$(DUNE) build src/verifier_bridge.bc.js
	cp $(JS_BUILD) $(WEB_VERIFIER)
	@echo "Created $(WEB_VERIFIER) from Rocq-extracted OCaml."

certified: web
	@echo "Certified web build complete."

# UI-only preview: uses the JavaScript mirror verifier and never displays CERTIFIED.
preview:
	@echo "Starting preview without Rocq extraction. CERTIFIED will remain disabled."
	cd web && $(PYTHON) -m http.server 8080

serve:
	@test -s $(WEB_VERIFIER) || { echo "Run 'make certified' first."; exit 1; }
	cd web && $(PYTHON) -m http.server 8080

clean:
	$(DUNE) clean
	rm -f $(WEB_VERIFIER) $(EXTRACTED_ML) src/calendar_verified.mli
	find $(ROCQ_DIR) -type f \( -name '*.vo' -o -name '*.vos' -o -name '*.vok' -o -name '*.glob' -o -name '*.aux' \) -delete

distclean: clean
