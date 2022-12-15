#include "day8_fut.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv) {
	int err;
	if (argc < 3) return -1;
	struct futhark_context_config *cfg = futhark_context_config_new();
	struct futhark_context *ctx = futhark_context_new(cfg);
	int fd = open(argv[2], 0, O_RDONLY);
	if (fd < 0) return errno;
	struct stat statbuf;
	if (err = fstat(fd, &statbuf)) return err;
	int len = statbuf.st_size;
	uint8_t* buf = malloc(len);
	if (!buf) return -2;
	int remaining_len = len;
	while (remaining_len > 0) {
		int read_val = read(fd, buf + (len - remaining_len), remaining_len);
//			printf("after read, val:%d", read_val);
		if (read_val < 0) return errno;
		if (read_val == 0) return -3;
		remaining_len -= read_val;
	}
	struct futhark_u8_1d * input = futhark_new_u8_1d(ctx, buf, len);
	switch (argv[1][0]) {
	case '1':
	{
		uint32_t output;
		int code = futhark_entry_part1(ctx, &output, input);
//		printf("after entry part1");
		if (code) return code;
		unsigned int output2 = output;
		printf("%u\n", output2);
	}
	break;
	case '2':
	{
		uint32_t output;
		int code = futhark_entry_part2(ctx, &output, input);
//		printf("after entry part2");
		if (code) return code;
		unsigned int output2 = output;
		printf("%u\n", output2);
	}
	break;
	}
}
