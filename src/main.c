#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
	if(argc != 2) {
		fprintf(stderr, "Incorrect usage. Correct usage:\n");
		fprintf(stderr, "molbc {input.molb}\n");

		return EXIT_FAILURE;
	}
	
	FILE *input;
	input = fopen(argv[1], "r");

	return EXIT_SUCCESS;
}
