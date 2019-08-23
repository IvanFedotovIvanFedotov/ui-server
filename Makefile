PROFILE   ?= release
BUILD     = dune build --profile $(PROFILE)
BUILD_DIR = _build/default
CLEAN     = dune clean
CSS_DIR   = ./dist/resources/css
CSS       = scss --load-path $(CSS_DIR) --style compressed

all: build

clean:
	$(CLEAN)

home:
	$(BUILD) frontend/home.bc.js
	cp $(BUILD_DIR)/frontend/home.bc.js dist/resources/js/home.js

input:
	$(BUILD) frontend/page_input.bc.js
	cp $(BUILD_DIR)/frontend/page_input.bc.js dist/resources/js/input.js

stream:
	$(BUILD) frontend/page_stream.bc.js
	cp $(BUILD_DIR)/frontend/page_stream.bc.js dist/resources/js/stream.js

topology:
	$(BUILD) application/pages_js/page_topology.bc.js
	cp $(BUILD_DIR)/application/pages_js/page_topology.bc.js dist/resources/js/topology.js

server_config:
	$(BUILD) frontend/server_config.bc.js
	cp $(BUILD_DIR)/frontend/server_config.bc.js dist/resources/js/server_config.js

user:
	$(BUILD) frontend/user.bc.js
	cp $(BUILD_DIR)/frontend/user.bc.js dist/resources/js/user.js

network:
	$(BUILD) frontend/network.bc.js
	cp $(BUILD_DIR)/frontend/network.bc.js dist/resources/js/network.js

demo:
	$(BUILD) frontend/demo/js/demo.bc.js
	cp $(BUILD_DIR)/frontend/demo/js/demo.bc.js dist/resources/js/demo.js

mosaic_editor:
	$(BUILD) pipeline/pages/mosaic_editor/js/page_mosaic_editor.bc.js
	cp $(BUILD_DIR)/pipeline/pages/mosaic_editor/js/page_mosaic_editor.bc.js dist/resources/js/mosaic_editor.js

mosaic_video:
	$(BUILD) pipeline/pages/mosaic_video/js/page_mosaic_video.bc.js
	cp $(BUILD_DIR)/pipeline/pages/mosaic_video/js/page_mosaic_video.bc.js dist/resources/js/mosaic_video.js

css-components:
	$(CSS) $(CSS_DIR)/@material/components/components.scss $(CSS_DIR)/components.min.css
	postcss -u autoprefixer -r $(CSS_DIR)/components.min.css

css-pages:
	$(CSS) $(CSS_DIR)/frontend/topology/topology.scss $(CSS_DIR)/topology.min.css
	$(CSS) $(CSS_DIR)/frontend/mosaic_video/mosaic_video.scss $(CSS_DIR)/mosaic_video.min.css
	$(CSS) $(CSS_DIR)/frontend/mosaic_editor/mosaic_editor.scss $(CSS_DIR)/mosaic_editor.min.css
	postcss -u autoprefixer -r $(CSS_DIR)/mosaic_editor.min.css
	$(CSS) $(CSS_DIR)/frontend/demo/demo.scss $(CSS_DIR)/demo.min.css
	$(CSS) $(CSS_DIR)/frontend/user/user.scss $(CSS_DIR)/user.min.css
	$(CSS) $(CSS_DIR)/main.scss $(CSS_DIR)/main.min.css

backend:
	$(BUILD) backend/backend.exe
	cp $(BUILD_DIR)/backend/backend.exe dist/backend

frontend: topology mosaic_editor mosaic_video
	@echo "Done"

css: css-components css-pages

build: backend home pipeline hardware user server_config network demo input stream
	@echo "Done"

dev: PROFILE = dev
dev: backend home pipeline hardware user server_config network demo input stream
	@echo "Done"

doc:
	odig odoc
	odig doc

test:
	dune runtest

.PHONY: build doc test all clean backend home pipeline hardware user network demo input stream
