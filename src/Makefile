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

day3: day3.o day3_fut.o day3_fut.h
	$(CC) $(LDFLAGS) -o day3 day3.o day3_fut.o

day3.o: day3.c day3_fut.h

day4: day4.o day4_fut.o day4_fut.h
	$(CC) $(LDFLAGS) -o day4 day4.o day4_fut.o

day4.o: day4.c day4_fut.h
