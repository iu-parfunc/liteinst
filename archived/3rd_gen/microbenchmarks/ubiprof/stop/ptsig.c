/* ptsig.c
 This program illustrates the use of signals in threads.
 
 Three threads including the main thread.
 main thread
 a. Set up a signal mask to block all signals.
 b. Set up signal handlers for SIGINT and SIGUSR1.
 c. Create thread_1, detached.
 d. Create thread_2, nondetached.
 e. Send SIGINT & SIGUSR1 to thread_1.
 f. Quit.
 
 thread_1
 a. Unblock all to embrace all signals.
 b. Wait for signals.
 c. Send SIGINT and SIGUSR1 to thread_2
 d. Wait for thread_2 to terminate
 e. Print thread_2 return status.
 f. Quit
 
 thread_2
 a. Unblock SIGUSR1 -- all others blocked due to inheritance.
 b. Wait for signals.
 c. Quit
 
 Note: There is hardly any error checking in this example -- not a good
 idea, but to make the program a bit more easier to explain.
 
 To compile:  gcc  ptsig.c -lpthread
 Sam Hsu (11/19/10)
 */

#define _POSIX_C_SOURCE 199506L
#include <stdio.h>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>

pthread_t tid2;
static void int_handler(int signo), usr1_handler(int signo);

void millisleep(int milliseconds)
{
    usleep(milliseconds * 1000);
}

main()
{
    pthread_t tid1;
    pthread_attr_t attr_obj;             /* a thread attribute variable */
    void *thread_1(void *), *thread_2(void *);
    sigset_t sigmask;
    struct sigaction action;
    
    /* set up signal mask to block all in main thread */
    sigfillset(&sigmask);                /* to turn on all bits */
    pthread_sigmask(SIG_BLOCK, &sigmask, (sigset_t *)0);
    
    /* set up signal handlers for SIGINT & SIGUSR1 */
    action.sa_flags = 0;
    action.sa_handler = int_handler;
    sigaction(SIGINT, &action, (struct sigaction *)0);
    action.sa_handler = usr1_handler;
    sigaction(SIGUSR1, &action, (struct sigaction *)0);
    
    pthread_attr_init(&attr_obj);        /* init it to default */
    pthread_attr_setdetachstate(&attr_obj, PTHREAD_CREATE_DETACHED);
    pthread_create(&tid1, &attr_obj, thread_1, (void *)NULL);
    printf("TID(%u) created\n", (unsigned int)tid1);
    
    pthread_attr_setdetachstate(&attr_obj, PTHREAD_CREATE_JOINABLE);
    pthread_create(&tid2, &attr_obj, thread_2, (void *)NULL);
    printf("TID(%u) created\n", (unsigned int)tid2);
    millisleep(1000);     /* for some reason a sleep is needed here */
    
    printf("main(%u) sending SIGINT to TID(%u)\n", (unsigned int)pthread_self(), (unsigned int)tid1);
    pthread_kill(tid1, SIGINT);          /* not blocked by tid1 */
    printf("main(%u) sending SIGUSR1 to TID(%u)\n", (unsigned int)pthread_self(), (unsigned int)tid1);
    pthread_kill(tid1, SIGUSR1);         /* not blocked by tid1 */
    
    printf("main(%u) is terminating\n", (unsigned int)pthread_self());
    pthread_exit((void *)NULL);          /* will not terminate process */
}  /* main */

void *thread_1(void *dummy)
{
    int sig, status, *status_ptr = &status;
    sigset_t sigmask;
    
    sigfillset(&sigmask);                /* will unblock all signals */
    pthread_sigmask(SIG_UNBLOCK, &sigmask, (sigset_t *)0);
    sigwait(&sigmask, &sig);
    switch(sig) {
        case SIGINT:
            int_handler(sig);
            break;
        default:
            break;
    }
    printf("TID(%u) sending SIGINT to %u\n", (unsigned int)pthread_self(), (unsigned int)tid2);
    pthread_kill(tid2, SIGINT);          /* blocked by tid2 */
    printf("TID(%u) sending SIGUSR1 to %u\n", (unsigned int)pthread_self(), (unsigned int)tid2);
    pthread_kill(tid2, SIGUSR1);         /* not blocked by tid2 */
    
    pthread_join(tid2, (void **)status_ptr);
    printf("TID(%u) exit status = %d\n", (unsigned int)tid2, status);
    
    printf("TID(%u) is terminating\n", (unsigned int)pthread_self());
    pthread_exit((void *)NULL);          /* calling thread will terminate */
}  /* thread_1 */

void *thread_2(void *dummy)
{
    int sig;
    sigset_t sigmask;
    
    sigemptyset(&sigmask);               /* to zero out all bits */
    sigaddset(&sigmask, SIGUSR1);        /* to unblock SIGUSR1 */
    pthread_sigmask(SIG_UNBLOCK, &sigmask, (sigset_t *)0);
    sigwait(&sigmask, &sig);
    switch(sig) {
        case SIGUSR1:
            usr1_handler(sig);
            break;
        default:
            break;
    }
    printf("TID(%u) is terminating\n", (unsigned int)pthread_self());
    pthread_exit((void *)NULL);          /* calling thread will terminate */
}  /* thread_2 */

static void int_handler(int dummy)
{
    printf("SIGINT received by TID(%u)\n", (unsigned int)pthread_self());
}  /* int_handler */

static void usr1_handler(int dummy)
{
    printf("SIGUSR1 received by TID(%u)\n", (unsigned int)pthread_self());
}
