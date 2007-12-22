#pragma warning(disable:4786) 
// warnings about long template names
// If compiling this file causes problems, try "rebuild all". This 
// is a problem coordinating boost with MS Visual studio 6
#include "boost/regex.hpp"
#include "pattern_match.h"
#include<vector>
#include<iostream>
static std::vector<int> match_ndx;

#define STDCALL 

extern "C" {


/**
 * Append a match to the list of matches. This is public in order to allow 
 * forced matches without bothering with regular expressions.
 */
void STDCALL append_match(int * ndx){
  match_ndx.push_back(*ndx);
}


/**
 *  Matches the matchstr argument against a regular expression pattern. You 
 *  can use 
 *  The 
 *  @arg ndx an index. The module will accumulate ndx of matches 
 *           until pattern_match_clear is called
 *  @arg matchstr the string to search
 *  @arg lenmatchstr length of the string to search
 *  @arg pattern the pattern to look for
 *  @arg length of the pattern
 */
void STDCALL pattern_match(int* ndx, char* matchstr,
				                       char* pattern,
									   int* istat,
									   int lenmatchstr,
									   int lenpattern)
{

   bool match=false;
   try{
	  std::string s(matchstr,lenmatchstr);
      boost::regex e(pattern,lenpattern,boost::regex_constants::normal);
      match=boost::regex_match(s,e);
   }catch(...){
      *istat=-1;
	  return;
   }
   if(match){
	  append_match(ndx);
      *istat=1;
   }else{
      *istat=0;
   }
}


/**
 * Retrieves the index (ndx) stored on the ith successful match since the 
 * match vector match_ndx was last cleared. Returns -901 if i is out of range
 */
int STDCALL  pattern_match_index(int * i){
   if (*i > (int)match_ndx.size()) return -901;
   return (int) match_ndx[*i-1];
}



/**
 * Reset the number of stored matching indexes to zero
 */
void STDCALL pattern_match_clear(){
	//std::cout << "clearing matcher"<<std::endl;
	match_ndx.clear();
}

int STDCALL pattern_match_count(){
	return match_ndx.size();
}

}
