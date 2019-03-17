PROFILE    ?= dev
BUILD      = dune build --profile $(PROFILE)
CLEAN      = dune clean
CSS        = scss --load-path dist/resources/css --style compressed
JS_TARGETS = home input stream mosaic_video mosaic_editor \
					   topo demo settings_user settings_network
CSS_DIR    = dist/resources/css

all: build

$(JS_TARGETS):
	$(BUILD) frontend/$@/js/$@.bc.js
	cp _build/default/frontend/$@/js/$@.bc.js dist/resources/js/$@.js

clean:
	$(CLEAN)

css-components:
	$(CSS) $(CSS_DIR)/@material/components/components.scss $(CSS_DIR)/components.min.css

css-pages:
	$(CSS) $(CSS_DIR)/main.scss $(CSS_DIR)/main.min.css
	$(CSS) $(CSS_DIR)/frontend/mosaic_video/mosaic_video.scss $(CSS_DIR)/mosaic_video.min.css
	$(CSS) $(CSS_DIR)/frontend/demo/demo.scss $(CSS_DIR)/demo.min.css
	$(CSS) $(CSS_DIR)/frontend/topology/topology.scss $(CSS_DIR)/topology.min.css
	$(CSS) $(CSS_DIR)/frontend/pipeline/pipeline.scss $(CSS_DIR)/pipeline.min.css
	$(CSS) $(CSS_DIR)/frontend/user/user.scss $(CSS_DIR)/user.min.css

css: css-components css-pages

backend:
	$(BUILD) backend/backend.exe
	cp _build/default/backend/backend.exe dist/backend

frontend: $(JS_TARGETS)

build: backend frontend
	@echo "Done"

dev: PROFILE = dev
dev: backend home frontend
	@echo "Done"

doc:
	odig odoc
	odig doc

test:
	dune runtest

.PHONY: build doc test all clean backend $(JS_TARGETS)
