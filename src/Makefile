CFLAGS:=$(CFLAGS) -g
LDFLAGS:=$(CFLAGS) -g
%.c %.h &: %.fut
	futhark c --library $<

day1: day1.o day1_fut.o day1_fut.h
	$(CC) $(LDFLAGS) -o day1 day1.o day1_fut.o

day1.o: day1.c day1_fut.h

day2: day2.o day2_fut.o day2_fut.h
	$(CC) $(LDFLAGS) -o day2 day2.o day2_fut.o

day2.o: day2.c day2_fut.h
