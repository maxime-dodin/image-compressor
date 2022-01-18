NAME	=	imageCompressor
package	=	imageCompressor

stack_yaml	=	STACK_YAML='stack.yaml'
stack	=	$(stack_yaml) stack
local_path	:=	$(shell stack path --local-install-root)
executable	:=	$(local_path)/bin

all: $(NAME)

$(NAME):
	$(stack) build $(package)
	cp "`stack path --local-install-root`/bin/imageCompressor-exe" $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all $(NAME) clean re