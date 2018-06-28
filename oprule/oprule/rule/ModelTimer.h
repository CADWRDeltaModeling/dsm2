#ifndef oprule_rule_MODELTIMER_H__INCLUDED_
#define oprule_rule_MODELTIMER_H__INCLUDED_


namespace oprule {
namespace rule {

/** Abstract timer interface to return model time.
 * @todo may not be used anymore?
 */
class ModelTimer  
{
public:
   virtual ~ModelTimer(){}
   /** Type use to represent time*/
   typedef int TimeType;
   /** Type use to represent time duration/time intervals*/
   typedef int DurationType;
   /** Return model time in time-system dependent ticks.*/
   virtual TimeType ticks()=0;
   /** Convert an integer number of minutes to the model duration type*/
   virtual DurationType minutes( int nmin )=0;
   /** Convert an integer number of hours to the model duration type*/
   virtual DurationType hours( int nhour )=0;
   /** Convert an integer number of days to the model duration type*/
   virtual DurationType days( int nday)=0;

};

}}     //namespace
#endif // include guard
