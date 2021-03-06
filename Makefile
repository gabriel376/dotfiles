.PHONY: install

install:
ifeq ($(shell id -u), 0)
	-useradd -m gabriel
	cp doas/doas.conf /etc/
else
	cp shell/profile ~/.profile
	cp -R cwm/ emacs/ fontconfig/ git/ ksh/ tmux/ X11/ ~/.config/
	cp -R bin/ ~/
endif
