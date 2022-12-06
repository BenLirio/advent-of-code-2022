#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main() {
	printf("%d\n", 3);
}

// 	movabsq	$.file_name, %rdi
//  mov $0, %rsi
//  callq open