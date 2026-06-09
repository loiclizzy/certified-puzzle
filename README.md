# Certified Puzzle

Author: CretiSen

This project provides an end-to-end formally verified workflow for the calendar puzzle:

```text
Rocq model and proofs
      │
      ├─ proves verify_sound
      ├─ checks 366 precomputed certificates
      └─ proves every_valid_date_has_solution
      │
      ▼ Extraction
Rocq generates OCaml: src/calendar_verified.ml
      │
      ▼ Dune + js_of_ocaml
Browser verifier: web/ocaml_verifier.js
      │
      ▼
The web app displays a solution and shows CERTIFIED only when the extracted verifier accepts it
```

The external solver is used only to find candidate certificates in advance. It is not part of the trusted base. Every certificate is checked again by the Rocq-defined verifier whose soundness is formally proved.

## Features

- Select any valid leap-year date, including February 29.
- Load one of 366 precomputed puzzle certificates.
- Verify the certificate with OCaml extracted from Rocq.
- Display a **CERTIFIED** badge only after successful extracted-verifier validation.
- Render the complete puzzle graphically.
- Guide the user through the solution one piece at a time.
- Copy the machine-verifiable certificate.
- Internationalized interface in:
  - French, the default language;
  - English;
  - Chinese;
  - Arabic (with right-to-left layout).

The selected language is stored in the browser's `localStorage`.

## Project layout

```text
calendar_puzzle_web_rocq_i18n/
├── Makefile
├── README.md
├── dune-project
├── rocq/
│   ├── CalendarPuzzle.v
│   ├── DateEnumeration.v
│   ├── GeneratedSolutions.v
│   ├── AllDatesProof.v
│   ├── Extract.v
│   └── _CoqProject
├── src/
│   ├── dune
│   └── verifier_bridge.ml
└── web/
    ├── index.html
    ├── styles.css
    ├── app.js
    ├── verifier_fallback.js
    ├── ocaml_verifier.js
    └── data/
        └── solutions.json
```

## Rocq source files

### `rocq/CalendarPuzzle.v`

Defines:

- the 43-cell board;
- the eight puzzle pieces and all allowed orientations;
- placements and certificates;
- the executable Boolean verifier `verify`;
- the mathematical specification `valid_solution`.

It proves the main soundness theorem:

```coq
Theorem verify_sound :
  forall month day cert,
    verify month day cert = true ->
    valid_solution month day cert.
```

Therefore, every certificate accepted by `verify` satisfies the formal puzzle specification.

### `rocq/DateEnumeration.v`

Defines the 366 valid dates of a leap year and the predicate:

```coq
Definition valid_date (month day : nat) : Prop :=
  In (month, day) valid_dates.
```

It also checks that the enumeration contains exactly 366 entries.

### `rocq/GeneratedSolutions.v`

Contains one precomputed certificate for each valid date.

This file is generated data. It does not need to be trusted because every entry is rechecked by Rocq.

### `rocq/AllDatesProof.v`

Defines certificate lookup and checks the complete table:

```coq
Theorem all_valid_dates_certified :
  all_valid_dates_certifiedb = true.
Proof.
  native_compute.
Qed.
```

It then proves the final completeness theorem:

```coq
Theorem every_valid_date_has_solution :
  forall month day,
    valid_date month day ->
    exists cert, valid_solution month day cert.
```

The proof structure is:

```text
valid date
  → the date occurs in valid_dates
  → the precomputed table contains a certificate
  → verify accepts the certificate
  → verify_sound proves valid_solution
```

### `rocq/Extract.v`

Configures extraction to OCaml and exports:

- `piece_id`;
- `placement`;
- `verify`;
- `solution_table`;
- `lookup_solution`;
- `certified_dateb`.

The generated file is:

```text
src/calendar_verified.ml
```

Do not edit this generated file manually. Regenerate it with `make extract`.

## Browser integration

### `src/verifier_bridge.ml`

This is a thin JavaScript/OCaml interoperability layer. It:

1. receives a JavaScript certificate;
2. converts it to the extracted OCaml placement type;
3. calls `Calendar_verified.verify`;
4. exposes the browser API:

```javascript
window.CalendarOCamlVerifier.verify(month, day, certificate)
```

The bridge identifies the verifier as a certified engine so that the web app can decide whether the **CERTIFIED** badge may be shown.

### `web/app.js`

The front end:

- loads `web/data/solutions.json`;
- selects the certificate for the requested date;
- invokes the OCaml verifier;
- renders the board and pieces;
- controls the step-by-step guide;
- manages French, English, and Chinese translations.

### `web/verifier_fallback.js`

This file provides a JavaScript mirror of the verifier for UI development when the Rocq/OCaml toolchain has not yet been built.

The fallback may be used for previewing the interface, but it never enables the official **CERTIFIED** badge.

## Requirements

Recommended versions:

- Rocq Prover 9.x;
- OCaml 4.14 or OCaml 5.x;
- opam;
- Dune 3.x;
- js_of_ocaml;
- js_of_ocaml-ppx;
- Python 3 for the local static HTTP server.

Install the OCaml-side dependencies with opam:

```bash
opam update
opam install dune js_of_ocaml js_of_ocaml-ppx
```

Install Rocq through opam or your operating system package manager. Then verify the tools:

```bash
rocq --version
ocamlc -version
dune --version
python3 --version
```

When using an opam switch, load its environment before building:

```bash
eval "$(opam env)"
```

If the Rocq executable is not named `rocq`, override it when invoking Make:

```bash
make certified ROCQ=/path/to/rocq
```

## Complete certified build

From the project root, run:

```bash
make certified
```

This is equivalent to:

```bash
make proof
make extract
make web
```

A successful build produces:

```text
src/calendar_verified.ml
web/ocaml_verifier.js
```

The first file is generated directly by Rocq Extraction. The second is the JavaScript browser verifier built from the extracted OCaml with Dune and js_of_ocaml.

## Step-by-step build

### 1. Check all Rocq proofs

```bash
make proof
```

The Makefile runs:

```bash
cd rocq && rocq compile CalendarPuzzle.v
cd rocq && rocq compile DateEnumeration.v
cd rocq && rocq compile GeneratedSolutions.v
cd rocq && rocq compile AllDatesProof.v
```

The expensive finite proof is performed by `native_compute` in `AllDatesProof.v`. It checks all 366 dates and all 366 stored certificates.

### 2. Extract the verified functions to OCaml

```bash
make extract
```

This first checks the proofs and then compiles:

```bash
cd rocq && rocq compile Extract.v
```

The relevant extraction declaration is:

```coq
Extraction Language OCaml.
Set Extraction Optimize.

Extraction TestCompile verify lookup_solution certified_dateb.

Extraction "../src/calendar_verified.ml"
  piece_id placement verify
  solution_table lookup_solution certified_dateb.
```

`Extraction TestCompile` performs a build-time smoke test of the extracted functions. The final extraction is then written to `src/calendar_verified.ml`.

### 3. Compile extracted OCaml to browser JavaScript

```bash
make web
```

The Makefile runs:

```bash
dune build src/verifier_bridge.bc.js
cp _build/default/src/verifier_bridge.bc.js web/ocaml_verifier.js
```

The Dune configuration is in `src/dune`:

```lisp
(executable
 (name verifier_bridge)
 (modules calendar_verified verifier_bridge)
 (modes js)
 (libraries js_of_ocaml)
 (preprocess (pps js_of_ocaml-ppx)))
```

The `calendar_verified` module is generated by Rocq. The `verifier_bridge` module only handles browser interoperability.

## Run the web application

After a certified build:

```bash
make serve
```

Open:

```text
http://localhost:8080
```

The application defaults to French. Use the language selector in the upper-right corner to switch to English or Chinese.

## UI-only preview

To preview the interface without Rocq Extraction or js_of_ocaml:

```bash
make preview
```

Then open:

```text
http://localhost:8080
```

In preview mode:

- date selection works;
- solutions are displayed;
- the guide works;
- the JavaScript mirror verifier runs;
- the official **CERTIFIED** badge remains disabled.

## Certification rule

The web application shows:

```text
✓ CERTIFIED
```

only when both conditions hold:

1. the loaded verifier identifies itself as the extracted OCaml engine;
2. the extracted `verify month day certificate` function returns `true`.

A successful verification proves, through `verify_sound`, that:

- all eight pieces are used exactly once;
- every orientation index is valid;
- exactly 41 cells are covered;
- no two pieces overlap;
- all covered cells belong to the board;
- the requested month and day remain visible.

## Certificate format

The web application displays certificates in the following format:

```text
# PIECE ORIENTATION ROW COLUMN
A 1 0 0
B 0 5 0
C 6 2 5
D 1 1 0
E 1 2 3
F 4 3 0
G 2 3 3
H 1 0 3
```

Each line contains:

```text
piece identifier, orientation index, anchor row, anchor column
```

Rows and columns are zero-based.

## Internationalization

The interface currently supports:

- `fr`: French, default;
- `en`: English;
- `zh`: Chinese.

Translations are stored in the `I18N` object in:

```text
web/app.js
```

To add a language:

1. add a translation object with the same keys as the French object;
2. add a matching `<option>` to the language selector in `web/index.html`;
3. test date formatting, guide instructions, buttons, verifier messages, and month names.

The language choice is persisted in `localStorage` under:

```text
calendarPuzzleLanguage
```

## Cleaning generated files

Remove Rocq build products, extracted OCaml, Dune output, and browser verifier:

```bash
make clean
```

`make distclean` currently performs the same cleanup.

## Trust model

The intended trusted base is:

- the Rocq kernel;
- the OCaml extraction mechanism;
- the OCaml and js_of_ocaml compilation toolchain;
- the small JavaScript/OCaml bridge;
- the browser runtime.

The external search program that produced the certificates is not trusted. It may generate arbitrary data; the build succeeds only when Rocq verifies every stored certificate.

## Typical workflow

```bash
# Install dependencies once
opam install dune js_of_ocaml js_of-ocaml-ppx

# Enter the project
cd calendar_puzzle_web_rocq_i18n

# Load the current opam switch
eval "$(opam env)"

# Check proofs, extract OCaml, and compile the browser verifier
make certified

# Start the web application
make serve
```

Then open `http://localhost:8080`.


## Branding

The project is branded as **Certified Puzzle** and includes the CertiSen logo in `web/assets/certified-puzzle-logo.png`.
The web UI theme is tuned to match the logo palette: deep blue background, white cards, and green verification accents.


## Manual builder and next-step recommendation

The web interface includes a manual construction mode:

- all eight piece shapes are rendered from the same orientation tables used by the verifier;
- pieces can be dragged from the tray to the board;
- placed pieces can be selected, moved, rotated, or removed;
- touch devices can select a piece and then tap a target cell;
- illegal partial placements are highlighted when they overlap, leave the board, or cover the selected month/day;
- the current manual certificate can be passed to the same OCaml verifier.

The **Suggest next step** feature analyses the pieces already placed. It compares the partial construction with the certified precomputed solution for the selected date. It first recommends correcting any placed piece whose orientation or anchor differs from the certified certificate; otherwise, it recommends the next missing piece and shows its shape, orientation, anchor, and covered cells. The recommendation can be applied directly to the manual board.

## Initial page behavior

The page does not display or generate a solution automatically on startup. The user first selects a month and day, then clicks **Generate and verify**. Changing either field clears the previously displayed solution until the button is clicked again.
## Guide-first workflow

The step-by-step guide is available immediately after the page loads. The user does not need to press **Generate and verify** to start.

- The guide starts at step `0 / 8`.
- Pressing **Next / Suivant / 下一步** reveals one certified piece placement at a time.
- The main board is constructed progressively as the guide advances.
- The current guide piece can be copied to the manual builder.
- **Generate and verify** remains optional: it reveals the complete solution, runs the verifier, and displays the certificate status.
- Changing the date resets the guide to step zero and loads the guide for the newly selected date.



## Guide startup behavior

The step-by-step guide is visible immediately on page load. The browser loads the precomputed certificate for the selected date internally, without revealing the complete solution. Users can click **Next / Suivant / 下一步** from step 0 without running **Generate and verify**. The generate button remains optional and is only used to reveal and verify the complete solution.
