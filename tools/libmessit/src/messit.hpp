
#include <unordered_map>
#include <string>

#include "probe_utils.hpp"

/**
 * {
 *  perturbation: [
 *    { 
 *      functions:...,*
 *      type: BUSY_WAIT, 
 *      duration: 10ms,
 *      trigger: 
 *        {
 *          frequency: 100ms,
 *          after: 3ms, ||
 *          onsignal: "Ctrl+D"
 *         }
 *    },
 *    {
 *      functions:...,*
 *      type: MEM_LEAK,
 *      size: 100b,
 *      duration: 10ms,
 *      trigger: 
 *        {
 *          frequency: 100ms,
 *          after: 3ms, ||
 *          onsignal: "Ctrl+D"
 *         }
 *    },
 *    {
 *      functions:...,*
 *      type: SLEEP, 
 *      duration: 10ms,
 *      trigger: 
 *        {
 *          frequency: 100ms,
 *          after: 3ms, ||
 *          onsignal: "Ctrl+D"
 *         }
 *    }
 *  ]
 *
 */

typedef std::unordered_map<std::string, Address> FuncAddrMapping;
typedef void (*perturbation_fn)();  

void messit();
