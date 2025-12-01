#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

enum TokenType {
	_return,
	intLiteral,
	semicolon
};

typedef struct {
	bool hasValue;
	char* value;
} OptionalString;

typedef struct {
	enum TokenType type;
	OptionalString value;
} Token;

#define T TokenVector, Token
#include <stc/vec.h>

char *readFile(const char *path) {
	FILE *input;
	input = fopen(path, "r");

	if(!input) {
		fprintf(stderr, "File %s does not exist\n", path);
	}

	fseek(input, 0, SEEK_END);
	long fileSize = ftell(input);
	rewind(input);

	char *contents = malloc(fileSize + 1);

	fread(contents, 1, fileSize, input);
	contents[fileSize] = '\0';

	fclose(input);

	return contents;
}

TokenVector tokenize(char* str) {
	for(int i = 0; str[i] != '\0'; i++) {
		printf("%c", str[i]);
	}
}

void freeToken();

int main(int argc, char* argv[]) {
	if(argc != 2) {
		fprintf(stderr, "Incorrect usage. Correct usage:\n");
		fprintf(stderr, "molbc {input.molb}\n");

		return EXIT_FAILURE;
	}
	
	char* contents = readFile(argv[1]);

	printf("content: %s\n\n", contents);
	tokenize(contents);

	free(contents);

	return EXIT_SUCCESS;
}
