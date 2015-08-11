
#ifndef _PROVIDER_REGISTRY_HPP_
#define _PROVIDER_REGISTRY_HPP_

#include "ProbeProvider.hpp"

class ProviderRegistry {

  public:

    // Registers a provider for given probe type
    /* \param type The probe type. Currently one of ZCA| FINSTRUMENT.
     * \param provider The probe provider instance
     */
    void registerProbe(ProbeType type, ProbeProvider* provider);

    // Gets the provider for given probe type
    /* \param type The probe type. Currently one of ZCA| FINSTRUMENT.
     */
    ProbeProvider* getProvider(ProbeType type);

}

#endif /* _PROVIDER_REGISTRY_HPP_ */
