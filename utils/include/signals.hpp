
#ifndef _SIGNALS_HPP_
#define _SIGNALS_HPP_

#include <signal.h>
#include <atomic>
#include <vector>

namespace utils {
namespace signals {

typedef int RegistrationId; ///< Opaque identifier for signal registration

typedef void (*SignalHandler)(int, siginfo_t *, void *);

constexpr int MAX_HANDLERS = 3;
constexpr int DEFAULT_PRIORITY = 3;

/// Describes a handler registration
class HandlerRegistration {
  friend class SignalHandlerRegistry;

  public:
    int signum; ///< The registered signal
    struct sigaction act; ///< Signal handler configuration as defined in signal.h
    struct sigaction oldact; ///< Old signal handler configuration
  private:
    RegistrationId reg_id; // Reistration id
    bool sync; ///< If the handler is registered synchronously
};

class SignalHandlerRegistry {
  public:

    /** \brief Enable handler invocation according to handler priority settings. 
     *  \param block_size The size of the block (typically page length)
     *  \param aligned    If the returned ranges are to be aligned to the block 
     *                    boundaries (i.e: The first block would start at a 
     *                    block aligned address instead of the start of the 
     *                    range if it happens to be lying inside a block). Same
     *                    for the range end (i.e last range would end at a 
     *                    block boundary).
     *                    Make it false to start the first range from the 
     *                    actual range start irrespective of the start address 
     *                    placement. Also the last range will not be aligned 
     *                    to the last block boundary. 
     *  \return Partitions of the range blocked to given block size.
     */
    static void enableHandlerPriority();

    /** \brief Disable handler invocation according to handler priority settings. 
     *
     * Disabled by default since it incurrs some additional overhead to sort 
     * hanlders according priority before invocation and isn't useful in cases 
     * where handlers may be invoked in any order. If enabled sometime during 
     * the runtime after some handlers have already been registered those will
     * also be subjected to priority based invocation in addition to the newly
     * registered handlers.
     */
    static void disableHandlerPriority();

    /** \brief Register a user signal handler
     *  \param reg  Handler registration information
     *  \param sync If this handler is to be registered as synchronously 
     *              invoked. The default value is true if unspecified.
     *  \param priority  Handler invocation priority if handler priority based
     *                   invocation is enabled. The default value corresponds to
     *                   the least handler priority if left unspecified.
     *
     * Registration might fail with an exception if either there is an 
     * error during underlying handler registration with OS or if the number of
     * user signal handlers would exceed the maximum number of handlers 
     * allowable for this signal. The default value is given by MAX_HANDLERS 
     * where such number of handlers can be registered separately for both as
     * synchroously and asynchronously innvoked for a given signal. The handler
     * priorities run from 1 to MAX_HANDLERS where 3 is the highest in priority
     * hence invoked last. The highest priority handler is invoked last so that
     * the it can potentially exit the signal handler with a longjump at the 
     * same time ensuring that other lower priority handlers have been invoked 
     * first. So it is important to ensure that only one of the user registered
     * handlers may transfer control with a longjump and that it is registered 
     * as the highest priority handler.
     */
    static void registerSignalHandler(HandlerRegistration& reg, 
        bool sync = true, int priority = DEFAULT_PRIORITY);

    /** \brief Unregister a user signal handler
     *  \param Handler registration information. This must be the same instance
     *    used for registering the user handler in a previous call to
     *    registerSignalHandler.
     */
    static void unregisterSignalHandler(HandlerRegistration& reg); 

  private:
    struct SigHandlerEntry {
      bool used;
      int priority;
      SignalHandler handler;
    };

    struct SigEntry {
      bool initialized;
      std::atomic_flag init_lock;
      std::atomic_flag reg_lock;
      int signum;
      int n_sync_handlers;
      int n_async_handlers;
      std::vector<SigHandlerEntry> sync_handlers;
      std::vector<SigHandlerEntry> async_handlers;
    };

    static bool priority_enabled;
    static SigEntry sig_entries[NSIG];

    static std::vector<SigHandlerEntry> handlerSort(int signum, bool sync);

    /** \brief Dispatcher which will dispatch to user registered handlers.
     *  \param signum  The signal which was asserted 
     *  \param siginfo Information about the signal as defined in signal.h 
     *  \param context Architecture dependent thread context information
     */
    static void handler_dispatcher(int signum, siginfo_t* siginfo,
        void* context); 

};

} /* End signals */
} /* End utils */

#endif /* _SIGNALS_HPP_ */
