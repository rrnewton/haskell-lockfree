// GCC atomic builtins, see http://gcc.gnu.org/onlinedocs/gcc-4.5.0/gcc/Atomic-Builtins.html

/* No size */
void mem_barrier (void);

/* 8-bit */
inline unsigned char fetch_and_add_8 (unsigned char *, unsigned char );
inline unsigned char fetch_and_sub_8 (unsigned char *, unsigned char );
inline unsigned char fetch_and_or_8 (unsigned char *, unsigned char );
inline unsigned char fetch_and_and_8 (unsigned char *, unsigned char );
inline unsigned char fetch_and_xor_8 (unsigned char *, unsigned char );
inline unsigned char fetch_and_nand_8 (unsigned char *, unsigned char );
inline unsigned char add_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned char sub_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned char or_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned char and_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned char xor_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned char nand_and_fetch_8 (unsigned char *, unsigned char );
inline unsigned int
bool_compare_and_swap_8 (unsigned char *, unsigned char, unsigned char);
inline unsigned char
val_compare_and_swap_8 (unsigned char *, unsigned char , unsigned char);
inline unsigned char lock_test_and_set_8 (unsigned char *);
void lock_release_8 (unsigned char *);

/* 16-bit */
inline unsigned short fetch_and_add_16 (unsigned short *, unsigned short );
inline unsigned short fetch_and_sub_16 (unsigned short *, unsigned short );
inline unsigned short fetch_and_or_16 (unsigned short *, unsigned short );
inline unsigned short fetch_and_and_16 (unsigned short *, unsigned short );
inline unsigned short fetch_and_xor_16 (unsigned short *, unsigned short );
inline unsigned short fetch_and_nand_16 (unsigned short *, unsigned short );
inline unsigned short add_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned short sub_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned short or_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned short and_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned short xor_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned short nand_and_fetch_16 (unsigned short *, unsigned short );
inline unsigned int
bool_compare_and_swap_16 (unsigned short *, unsigned short , unsigned short );
inline unsigned short
val_compare_and_swap_16 (unsigned short *, unsigned short , unsigned short );
inline unsigned short lock_test_and_set_16 (unsigned short *);
void lock_release_16 (unsigned short *);


/* 32-bit */
inline unsigned int fetch_and_add_32 (unsigned int *, unsigned int );
inline unsigned int fetch_and_sub_32 (unsigned int *, unsigned int );
inline unsigned int fetch_and_or_32 (unsigned int *, unsigned int );
inline unsigned int fetch_and_and_32 (unsigned int *, unsigned int );
inline unsigned int fetch_and_xor_32 (unsigned int *, unsigned int );
inline unsigned int fetch_and_nand_32 (unsigned int *, unsigned int );
inline unsigned int add_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int sub_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int or_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int and_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int xor_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int nand_and_fetch_32 (unsigned int *, unsigned int );
inline unsigned int
bool_compare_and_swap_32 (unsigned int *, unsigned int , unsigned int );
inline unsigned int
val_compare_and_swap_32 (unsigned int *, unsigned int , unsigned int );
inline unsigned int lock_test_and_set_32 (unsigned int *);
void lock_release_32 (unsigned int *);

/* 64-bit */
inline unsigned long long fetch_and_add_64 (unsigned long long *, unsigned long long );
inline unsigned long long fetch_and_sub_64 (unsigned long long *, unsigned long long );
inline unsigned long long fetch_and_or_64 (unsigned long long *, unsigned long long );
inline unsigned long long fetch_and_and_64 (unsigned long long *, unsigned long long );
inline unsigned long long fetch_and_xor_64 (unsigned long long *, unsigned long long );
inline unsigned long long fetch_and_nand_64 (unsigned long long *, unsigned long long );
inline unsigned long long add_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned long long sub_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned long long or_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned long long and_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned long long xor_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned long long nand_and_fetch_64 (unsigned long long *, unsigned long long );
inline unsigned int
bool_compare_and_swap_64 (unsigned long long *, unsigned long long , unsigned long long );
inline unsigned long long
val_compare_and_swap_64 (unsigned long long *, unsigned long long , unsigned long long );
inline unsigned long long lock_test_and_set_64 (unsigned long long *);
void lock_release_64 (unsigned long long *);

/* Word */
inline unsigned long fetch_and_add_word (unsigned long *, unsigned long );
inline unsigned long fetch_and_sub_word (unsigned long *, unsigned long );
inline unsigned long fetch_and_or_word (unsigned long *, unsigned long );
inline unsigned long fetch_and_and_word (unsigned long *, unsigned long );
inline unsigned long fetch_and_xor_word (unsigned long *, unsigned long );
inline unsigned long fetch_and_nand_word (unsigned long *, unsigned long );
inline unsigned long add_and_fetch_word (unsigned long *, unsigned long );
inline unsigned long sub_and_fetch_word (unsigned long *, unsigned long );
inline unsigned long or_and_fetch_word (unsigned long *, unsigned long );
inline unsigned long and_and_fetch_word (unsigned long *, unsigned long );
inline unsigned long xor_and_fetch_word (unsigned long *, unsigned long );
inline unsigned long nand_and_fetch_word (unsigned long *, unsigned long );
inline unsigned int
bool_compare_and_swap_word (unsigned long *, unsigned long , unsigned long );
inline unsigned long
val_compare_and_swap_word (unsigned long *, unsigned long , unsigned long );
inline unsigned long lock_test_and_set_word (unsigned long *);
void lock_release_word (unsigned long *);
