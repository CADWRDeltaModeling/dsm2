/**
 * Population.hpp
 *
 * Implementation of Population class
 * Created on: Jun 16, 2009
 * Author: Kijin Nam, knam@water.ca.gov
 */

#include "boost/foreach.hpp"

namespace PTM2
{

template <typename ParticleT>
Population<ParticleT>::Population()
{

}



template <typename ParticleT>
Population<ParticleT>::~Population()
{
	clearPopulation();
}



template <typename ParticleT>
void
Population<ParticleT>::addParticle(ParticleType* pP)
{
	assert(pP!=NULL);
	this->push_back(pP);
}



template <typename ParticleT>
void
Population<ParticleT>::clearPopulation()
{
	ParticleT* pP;
	BOOST_FOREACH( pP, *this )
	{
		if ( pP != NULL ) {
			delete pP;
			pP = NULL;
		}
	}
}

} // namespace PTM2
