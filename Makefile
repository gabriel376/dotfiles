.PHONY: install

install:
ifeq ($(shell id -u), 0)
	-useradd -m gabriel
else

endif
