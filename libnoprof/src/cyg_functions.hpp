
#ifndef _CYG_FUNCTIONS_HPP_
#define _CYG_FUNCTIONS_HPP_

extern "C"
{
  void __cyg_profile_func_enter(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));

  void __cyg_profile_func_exit(void *this_fn, void *call_site)
    __attribute__((no_instrument_function));
}
#endif /* _CYG_FUNCTIONS_HPP__ */
