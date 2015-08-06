/**
 * @file bitmao.hpp
 * @author Buddhika Chamith
 * @brief Interface to bitmap. 
 *
 */

#include <cstdint>

//BJS: Should probably do 
typedef uint8_t* Bitmap;


//BJS: Maybe also do: 

/**
 * @brief Create a bitmap with num_bits number of bit positions 
 * @param num_bits
 * @return a Bitmap if successful, NULL otherwise
 */
Bitmap create_bitmap(unsigned int num_bits); 

/**
 * @brief Destroy a bitmap 
 * @param b the Bitmap to destroy. 
 */
void destroy_bitmap(Bitmap b);

//BJS: Buddhika improve on these doxycomments.

/**
 * @brief Set a bit in a bitmap. 
 * @param bitmap 
 * @param idx bit to set
 */
void set_index(Bitmap bitmap, int idx);

/**
 * @brief read a bit from a bitmap.
 * @param bitmap
 * @param idx bit to read 
 * @return 1 if bit is set, 0 otherwise. 
 */
int get_index(Bitmap bitmap, int idx); 
