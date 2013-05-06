#include "atomic-bitops-gcc.h"

void mem_barrier (void) {return __sync_synchronize ();}

/* 8-bit */
inline unsigned char fetch_and_add_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_add (p, v);
}
inline unsigned char fetch_and_sub_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_sub (p, v);
}
inline unsigned char fetch_and_or_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_or (p, v);
}
inline unsigned char fetch_and_and_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_and (p, v);
}
inline unsigned char fetch_and_xor_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_xor (p, v);
}
inline unsigned char fetch_and_nand_8 (unsigned char *p, unsigned char v) {
	return __sync_fetch_and_nand (p, v);
}
inline unsigned char add_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_add_and_fetch (p, v);
}
inline unsigned char sub_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_sub_and_fetch (p, v);
}
inline unsigned char or_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_or_and_fetch (p, v);
}
inline unsigned char and_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_and_and_fetch (p, v);
}
inline unsigned char xor_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_xor_and_fetch (p, v);
}
inline unsigned char nand_and_fetch_8 (unsigned char *p, unsigned char v) {
	return __sync_nand_and_fetch (p, v);
}
inline unsigned int
bool_compare_and_swap_8 (unsigned char *p, unsigned char old, unsigned char new) {
	return __sync_bool_compare_and_swap (p, old, new);
}
inline unsigned char
val_compare_and_swap_8 (unsigned char *p, unsigned char old, unsigned char new) {
	return __sync_val_compare_and_swap (p, old, new);
}
inline unsigned char lock_test_and_set_8 (unsigned char *p) {
	// Only immediate 0/1 appear to be widely supported, so hardcode it
	// here
	return __sync_lock_test_and_set (p, 1);
}
void lock_release_8 (unsigned char *p) {
	// Writes a 0 to *p
	return __sync_lock_release (p);
}

/* 16-bit */
inline unsigned short fetch_and_add_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_add (p, v);
}
inline unsigned short fetch_and_sub_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_sub (p, v);
}
inline unsigned short fetch_and_or_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_or (p, v);
}
inline unsigned short fetch_and_and_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_and (p, v);
}
inline unsigned short fetch_and_xor_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_xor (p, v);
}
inline unsigned short fetch_and_nand_16 (unsigned short *p, unsigned short v) {
	return __sync_fetch_and_nand (p, v);
}
inline unsigned short add_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_add_and_fetch (p, v);
}
inline unsigned short sub_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_sub_and_fetch (p, v);
}
inline unsigned short or_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_or_and_fetch (p, v);
}
inline unsigned short and_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_and_and_fetch (p, v);
}
inline unsigned short xor_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_xor_and_fetch (p, v);
}
inline unsigned short nand_and_fetch_16 (unsigned short *p, unsigned short v) {
	return __sync_nand_and_fetch (p, v);
}
inline unsigned int
bool_compare_and_swap_16 (unsigned short *p, unsigned short old, unsigned short new) {
	return __sync_bool_compare_and_swap (p, old, new);
}
inline unsigned short
val_compare_and_swap_16 (unsigned short *p, unsigned short old, unsigned short new) {
	return __sync_val_compare_and_swap (p, old, new);
}
inline unsigned short lock_test_and_set_16 (unsigned short *p) {
	// Only immediate 0/1 appear to be widely supported, so hardcode it
	// here
	return __sync_lock_test_and_set (p, 1);
}
void lock_release_16 (unsigned short *p) {
	// Writes a 0 to *p
	return __sync_lock_release (p);
}

/* 32-bit */
inline unsigned int fetch_and_add_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_add (p, v);
}
inline unsigned int fetch_and_sub_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_sub (p, v);
}
inline unsigned int fetch_and_or_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_or (p, v);
}
inline unsigned int fetch_and_and_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_and (p, v);
}
inline unsigned int fetch_and_xor_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_xor (p, v);
}
inline unsigned int fetch_and_nand_32 (unsigned int *p, unsigned int v) {
	return __sync_fetch_and_nand (p, v);
}
inline unsigned int add_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_add_and_fetch (p, v);
}
inline unsigned int sub_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_sub_and_fetch (p, v);
}
inline unsigned int or_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_or_and_fetch (p, v);
}
inline unsigned int and_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_and_and_fetch (p, v);
}
inline unsigned int xor_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_xor_and_fetch (p, v);
}
inline unsigned int nand_and_fetch_32 (unsigned int *p, unsigned int v) {
	return __sync_nand_and_fetch (p, v);
}
inline unsigned int
bool_compare_and_swap_32 (unsigned int *p, unsigned int old, unsigned int new) {
	return __sync_bool_compare_and_swap (p, old, new);
}
inline unsigned int
val_compare_and_swap_32 (unsigned int *p, unsigned int old, unsigned int new) {
	return __sync_val_compare_and_swap (p, old, new);
}
inline unsigned int lock_test_and_set_32 (unsigned int *p) {
	// Only immediate 0/1 appear to be widely supported, so hardcode it
	// here
	return __sync_lock_test_and_set (p, 1);
}
void lock_release_32 (unsigned int *p) {
	// Writes a 0 to *p
	return __sync_lock_release (p);
}

/* 64-bit */
inline unsigned long long fetch_and_add_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_add (p, v);
}
inline unsigned long long fetch_and_sub_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_sub (p, v);
}
inline unsigned long long fetch_and_or_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_or (p, v);
}
inline unsigned long long fetch_and_and_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_and (p, v);
}
inline unsigned long long fetch_and_xor_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_xor (p, v);
}
inline unsigned long long fetch_and_nand_64 (unsigned long long *p, unsigned long long v) {
	return __sync_fetch_and_nand (p, v);
}
inline unsigned long long add_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_add_and_fetch (p, v);
}
inline unsigned long long sub_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_sub_and_fetch (p, v);
}
inline unsigned long long or_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_or_and_fetch (p, v);
}
inline unsigned long long and_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_and_and_fetch (p, v);
}
inline unsigned long long xor_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_xor_and_fetch (p, v);
}
inline unsigned long long nand_and_fetch_64 (unsigned long long *p, unsigned long long v) {
	return __sync_nand_and_fetch (p, v);
}
inline unsigned int
bool_compare_and_swap_64 (unsigned long long *p, unsigned long long old, unsigned long long new) {
	return __sync_bool_compare_and_swap (p, old, new);
}
inline unsigned long long
val_compare_and_swap_64 (unsigned long long *p, unsigned long long old, unsigned long long new) {
	return __sync_val_compare_and_swap (p, old, new);
}
inline unsigned long long lock_test_and_set_64 (unsigned long long *p) {
	// Only immediate 0/1 appear to be widely supported, so hardcode it
	// here
	return __sync_lock_test_and_set (p, 1);
}
void lock_release_64 (unsigned long long *p) {
	// Writes a 0 to *p
	return __sync_lock_release (p);
}

/* Word-sized */
inline unsigned long fetch_and_add_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_add (p, v);
}
inline unsigned long fetch_and_sub_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_sub (p, v);
}
inline unsigned long fetch_and_or_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_or (p, v);
}
inline unsigned long fetch_and_and_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_and (p, v);
}
inline unsigned long fetch_and_xor_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_xor (p, v);
}
inline unsigned long fetch_and_nand_word (unsigned long *p, unsigned long v) {
	return __sync_fetch_and_nand (p, v);
}
inline unsigned long add_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_add_and_fetch (p, v);
}
inline unsigned long sub_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_sub_and_fetch (p, v);
}
inline unsigned long or_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_or_and_fetch (p, v);
}
inline unsigned long and_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_and_and_fetch (p, v);
}
inline unsigned long xor_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_xor_and_fetch (p, v);
}
inline unsigned long nand_and_fetch_word (unsigned long *p, unsigned long v) {
	return __sync_nand_and_fetch (p, v);
}
inline unsigned int
bool_compare_and_swap_word (unsigned long *p, unsigned long old, unsigned long new) {
	return __sync_bool_compare_and_swap (p, old, new);
}
inline unsigned long
val_compare_and_swap_word (unsigned long *p, unsigned long old, unsigned long new) {
	return __sync_val_compare_and_swap (p, old, new);
}
inline unsigned long lock_test_and_set_word (unsigned long *p) {
	// Only immediate 0/1 appear to be widely supported, so hardcode it
	// here
	return __sync_lock_test_and_set (p, 1);
}
void lock_release_word (unsigned long *p) {
	// Writes a 0 to *p
	return __sync_lock_release (p);
}
