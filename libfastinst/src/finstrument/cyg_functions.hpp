
/**
 *  @file cyg_functions.hpp
 *  @author Buddhika Chamith, Bo Joel Svensson, Ryan Newton
 *  @brief cyg_profile_function_enter/exit and related definitions.
 * 
 */

#ifndef _CYG_FUNCTIONS_HPP_
#define _CYG_FUNCTIONS_HPP_

extern "C"
{
    /**
     *  @brief Ubiprof provided implementation of __cyg_profile_func_enter.
     *  @param this_fn Address of current function  
     *  @param call_site Cyg function call site. This is next instruction address
     *  that immediately follows the call to the cyg function. 
     */
    void __cyg_profile_func_enter(void *this_fn, void *call_site)
          __attribute__((no_instrument_function));

    /**
     * @brief Ubiprof provided implementation of __cyg_profile_func_enter.
     *  @param this_fn Address of current function  
     *  @param call_site Cyg function call site. This is next instruction address
     *  that immediately follows the call to the cyg function. 
     */
    void __cyg_profile_func_exit(void *this_fn, void *call_site)
          __attribute__((no_instrument_function));

    // BJS: These break abstraction.
    void fake_cyg_profile_func_enter(void *this_fn, void *call_site)
         __attribute__((no_instrument_function));
    void fake_cyg_profile_func_exit(void *this_fn, void *call_site)
         __attribute__((no_instrument_function));
   
}

#endif /* _CYG_FUNCTIONS_HPP__ */
