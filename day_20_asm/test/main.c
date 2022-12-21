#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
int main() {
	void* v = mmap(0, 512, 3, 34, 0, 0);
	printf("%p\n", v);
}
