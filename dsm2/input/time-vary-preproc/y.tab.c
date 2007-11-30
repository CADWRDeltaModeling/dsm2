
# line 2 "ascii2dss.y"
# include <alloca.h>
# include <ctype.h>   /* This has function isspace() */
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
  
  /* Some constants we need. */
#define LINESIZE 200
#define INSTRLEN  50     /* The maximum length of an input string */
#define INSHORTSTRLEN 11 /* The maximum length of short input strings */
#define NIPAT 9 /* The number of initial information units that aren't compiled */  
#define COMMENT_SYMBOL '#'
  

  /* global C variables 
   */
  
  /* The different time patterns we recognize. */ 
  enum {exact, set, repeat, star} timepattern; 
  
     /* The different time step */				     
  enum {irregular, minutes, hours, days, weeks, months, years} timesteps;
  
  typedef struct timeunit
    {   unsigned long int tu_time;
	struct timeunit *tu_next;
      } timeunit;

  typedef struct timeunit ELEMENT;
  
  static ELEMENT *hours_in,
  *days_in,
  *months_in,
  *years_in,
  *timelist;

  char location[INSTRLEN],
  step[INSHORTSTRLEN],
  area[INSTRLEN], 
  data_type[INSTRLEN], 
  data_units[INSTRLEN], 
  study[INSTRLEN], 
  sdate[INSHORTSTRLEN],
  stime[INSHORTSTRLEN],
  edate[INSHORTSTRLEN],
  etime[INSHORTSTRLEN],
  value[INSTRLEN], 
  dssfile[INSTRLEN],
  instr[INSTRLEN];
  
  FILE *fp;
  
  int pos = -1,
  lineno = 1,
  EVALUATE = 1;

/************************************************************
 * function  : freelist
 * purpose   : frees up the memory used by a list
 ************************************************************/
void freelist(list)
     ELEMENT *list;
{
  ELEMENT *l, *dead;
  
  l = list->tu_next;
  while (l != NULL)
    { 
      dead = l;
      l = l->tu_next;
      free(dead);
    }
  list->tu_next = NULL;
}

/************************************************************
 * function  : create_element
 * purpose   : allocates a list element storage unit
 ************************************************************/
ELEMENT *create_element()
{
  ELEMENT *l;
  
  l = (ELEMENT *)malloc(sizeof(ELEMENT));
  if (l == NULL)
    {
      fprintf(stderr,"create_element: malloc failed.\n");
      exit(1);
    }
  l->tu_next == NULL;
  return(l);
}

/************************************************************
 * function  : append
 * purpose   : appends an element to a list.
 ************************************************************/
void append(list,elmt)
     ELEMENT *list, *elmt;
{
  ELEMENT *l;
  
  /* Is the list empty ? */
  if (list == NULL)
    { 
      list = elmt;
      return;
    }

  /* list is not empty.  Go to its end and append element there */
  for (l = list; l->tu_next != NULL; l = l->tu_next)
    ;
  l->tu_next = elmt;
  elmt->tu_next = NULL;
}

/************************************************************
 * function  : prtlist
 * purpose   : prints the date field of each element in a list
 ************************************************************/
void prtlist(list)
     ELEMENT *list;
{
  ELEMENT *l;
  l = list;
  while (l = l->tu_next)
    printf("%d ",l->tu_time);
  printf("\n");
  return;
}

/*****************************************************************
 * function: skipline
 * purpose : same as yyerror, but no diagnostic messages
 ******************************************************************/
void skipline() 
{  
  char line[LINESIZE];
  
  if ( fgets( line, LINESIZE - 1, fp ) != NULL )
    { 
      lineno++;
      pos = -1;
      EVALUATE = 1;
      freelist(timelist);
      freelist(hours_in);
      freelist(days_in);
      freelist(months_in);
      freelist(years_in);
      location[INSTRLEN],
      step[INSHORTSTRLEN],
      area[0] = '\0';
      data_type[0] = '\0';
      data_units[0] = '\0';
      study[0] = '\0';
      sdate[0] = '\0';
      stime[0] = '\0';
      edate[0] = '\0';
      etime[0] = '\0';
      value[0] = '\0';
      return;
    }
}

/************************************************************
 * function  : nelements
 * purpose   : The number of elements in a list
 ************************************************************/
int nelements(list)
     ELEMENT *list;
{
  ELEMENT *l;
  int i = 0;

  l = list;
  while (l = l->tu_next)
    i++;
  return(i);
}

/************************************************************
 * function  : swap
 * purpose   : Swaps the two input integer values
 ************************************************************/
void swap(x,y)
     int *x, *y;

{
  int tmp;
  
  tmp = *x;
  *x = *y;
  *y = tmp;
}

/************************************************************
 * function  : sortlist
 * purpose   : Uses bubble sort to sort list.
 ************************************************************/
ELEMENT *sortlist(list)
     ELEMENT *list;
{
  ELEMENT *curr, *key;
  
#if DEBUG  
  printf("In sortlist\n");
  prtlist(list);
#endif

  key = list;
  
  while (key = key->tu_next)
    {
      if (key->tu_next != NULL)
	{
	  curr = key->tu_next;
	  
	  while (curr != NULL)
	    {
	      if (curr->tu_time == key->tu_time)
		{
		  yyerror("Duplicate input not allowed");
		  return(0);
		}
	      if (curr->tu_time < key->tu_time)
		swap(&curr->tu_time,&key->tu_time);
	      curr = curr->tu_next;
	    }
	}
    }
  
#if DEBUG  
  prtlist(list);
#endif
  
  return(list);
}

/************************************************************
 * function  : strseq
 * purpose   : Determines if two strings are equal.
 ************************************************************/
int strseq(str1,str2)
      char *str1, *str2;
 {
   if (!strcmp(str1,str2))
     return(1);
   return(0);
 }

/************************************************************
 * function  : substr
 * purpose   : Given string str and positions s and e returns str(s:e)
 ************************************************************/
char *substr(str,s,e)
     char *str; 
     int s, e;
{ 
  char *cpy;
  cpy = (char *) calloc(INSHORTSTRLEN,sizeof(char));
  strcpy(cpy,str);
  cpy[++e] = '\0';
  return(cpy + s);
}

/************************************************************
 * function  : int2string
 * purpose   : Converts an integer to its string representation 
 ************************************************************/
char *int2string(num)
     int num;
{
  char *str, *curr, *end, *sdiv;
  int itemp, intlen = 0, intlen2 = 0,  digit, div;
  
  itemp =  num;

  if (itemp)

    while ( (itemp /= 10) >= 1 )
      intlen++;
  
  div = 1;
  intlen2 = intlen;
  while (intlen2--)
    div *= 10;
  
  str = (char *)malloc((intlen + 1)*sizeof(char));
  curr = str;
  end = str + intlen + 1;
  while (curr != end)
    {
      digit = num / div;
      num = num % div;
      switch (digit)
	{
	case 0: *curr = '0';  
	  break;
	case 1: *curr = '1';  
	  break;
	case 2: *curr = '2';  
	  break;
	case 3: *curr = '3';  
	  break;
	case 4: *curr = '4';  
	  break;
	case 5: *curr = '5';  
	  break;
	case 6: *curr = '6';  
	  break;
	case 7: *curr = '7';  
	  break;
	case 8: *curr = '8';  
	  break;
	case 9: *curr = '9';  
	  break;
	default: *curr = '0';  
	  break;
	}
      div = div / 10;
      curr++;
    }
  *curr = '\0';
  return(str);
}

/************************************************************
 * function  : str2upper
 * purpose   : Returns uppercase of a string
 ************************************************************/
char *str2upper(str)
     char *str;
{ char *temp;

  temp = str;
  while (*temp != '\0')
    {
      *temp = toupper(*temp);
      temp++;
    }
  return(str);
}       

/************************************************************
 * function  : estep
 * purpose   : Returns the enumerated position of the time step
 ************************************************************/
int estep(step)
     char *step;
{
  if (!strcmp(step,"15MIN"))
   return(minutes);
  else if (!strcmp(step,"1HOUR"))
    return(hours);
  else if (!strcmp(step,"1DAY"))
    return(days);
  else if (!strcmp(step,"1WEEK"))
    return(weeks);
  else if (!strcmp(step,"1MONTH"))
    return(months);
  else if (!strcmp(step,"1YEAR"))
    return(years);
  else if (!strcmp(substr(step,0,3),"IR-"))
    return(irregular);
}

/************************************************************
 * function  : stepsize
 * purpose   : Returns the number of minutes coressponding to
 *             each regular step size.
 ************************************************************/
unsigned long int stepsize(step)
     char *step;
{
  int mins = 0;
  step = str2upper(step);
  
  if (!strcmp(step,"15MIN"))
    mins = 15;
  else if (!strcmp(step,"1HOUR"))
    mins = 60;
  else if (!strcmp(step,"1DAY"))
    mins = 1440;
  else if (!strcmp(step,"1WEEK"))
    mins = 10080;
  else if (!strcmp(substr(step,0,3),"IR-"))
    mins = 0;
  return(mins);
}

/************************************************************
 * function  : date2days
 * purpose   : Converts date to number of days since 1900
 ************************************************************/
unsigned long int date2days(day, month, year)
     int day, month, year;
{
  /* the number of days since the beginning of the year for the end of each month */
  static int mdays[12] = {0,31,59,90,120,151,181,212,243,273,304,334};
  /* the number of days in each month */
  static int days_in_month[13] = {0,31,29,31,30,31,30,31,31,30,31,30,31};
  /* We will subtract the number of days from year 0000 to 1990 */
  static unsigned long int days, base = 693960;
  long int pyear, leaps, leap = 0;
  
  if ( (month < 1) || (month > 12))
    {
      yyerror("Invalid month");
      return(0);
    }
  if ( ( 0 >= day) || (days_in_month[month] < day) )
    {
      yyerror("Invalid number of days for the month entered");
      return(0);
    }
  if (month == 2) 
    if (day == 29) 
      if ( (year % 4) || !(year % 100) )
	{
	  yyerror("Entered year not a leap year");
	  return(0);
	}
        
  pyear = year - 1;
  leaps = pyear/4 + pyear/400 - pyear/100;

  if ( month >= 3 )
    if ( ( !(year % 4) && (year % 100) ) || 
	( !(year % 100) && !(year % 400) ) )
      leap = 1;
  
  days = 365*year+leaps+mdays[month-1]+day+leap-base;
  return(days);
}

/************************************************************
 * function  : day2value
 * purpose   : Converts day to it's integer value and 
 *              Mon, Tue, Wed, Thu, Fri, Sat, and Sun to
 *               1    2    3    4    5    6   and  7
 ************************************************************/
int day2value(day)
     char *day;

{ 
  if (atoi(day))
    return(atoi(day));
  else if (strlen(day) >= 3)
    {
      if (!strcmp(substr(str2upper(day),0,2),"MON"))
        return(1);
      else if (!strcmp(substr(str2upper(day),0,2),"TUE"))
	return(2);
      else if (!strcmp(substr(str2upper(day),0,2),"WED"))
	return(3);
      else if (!strcmp(substr(str2upper(day),0,2),"THU"))
	return(4);
      else if (!strcmp(substr(str2upper(day),0,2),"FRI"))
	return(5);
      else if (!strcmp(substr(str2upper(day),0,2),"SAT"))
	return(6);
      else if (!strcmp(substr(str2upper(day),0,2),"SUN"))
	return(7);
    }
  fprintf(stderr,"Unrecognized day %s\n",day);
  fprintf(stderr,"Can't process this record \n");
  yyerror("Invalid element specified");
  return(0);
}

/************************************************************
 * function  : month2value
 * purpose   : Converts MONTH to its integer value 
 *              JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC
 *	        1   2   3   4   5   6   7   8   9  10  11  12.
 ************************************************************/
int month2value(month)
     char *month;

{
  if (atoi(month))
    return(atoi(month));
  else if (strlen(month) >= 3)
    {
      if (!strcmp(substr(str2upper(month),0,2),"JAN"))
	return(1);
      else if (!strcmp(substr(str2upper(month),0,2),"FEB"))
	return(2);
      else if (!strcmp(substr(str2upper(month),0,2),"MAR"))
	return(3);
      else if (!strcmp(substr(str2upper(month),0,2),"APR"))
	return(4);
      else if (!strcmp(substr(str2upper(month),0,2),"MAY"))
	return(5);
      else if (!strcmp(substr(str2upper(month),0,2),"JUN"))
	return(6);
      else if (!strcmp(substr(str2upper(month),0,2),"JUL"))
	return(7);
      else if (!strcmp(substr(str2upper(month),0,2),"AUG"))
	return(8);
      else if (!strcmp(substr(str2upper(month),0,2),"SEP"))
	return(9);
      else if (!strcmp(substr(str2upper(month),0,2),"OCT"))
	return(10);
      else if (!strcmp(substr(str2upper(month),0,2),"NOV"))
	return(11);
      else if (!strcmp(substr(str2upper(month),0,2),"DEC"))
        return(12);
    }
  fprintf(stderr,"Unrecognized month %s\n",month);
  fprintf(stderr,"Can't process this record \n");
  yyerror("Invalid element specified");
  return(0);
}

/************************************************************
 * function  : range
 * purpose   : Determines the range of the current event in minutes.
 ************************************************************/
void range(start,end)
     unsigned long int *start, *end;

{	
  int shour, sday, smon, syear, ehour, eday, emon, eyear;
  unsigned long int shours, ehours;

  sday = atoi(substr(sdate,3,4));
  smon = atoi(substr(sdate,0,1));
  syear = atoi(substr(sdate,6,9));
  shour = atoi(stime);
  shours =  1440*date2days(sday,smon,syear);
  if (!shours) 
    return;
  *start = shours + 60*(shour / 100) + (shour % 100);

  eday = atoi(substr(edate,3,4));
  emon = atoi(substr(edate,0,1));
  eyear = atoi(substr(edate,6,9));
  ehour = atoi(etime);
  ehours =  1440*date2days(eday,emon,eyear);
  if (!ehours) 
    return;
  *end = ehours + 60*(ehour / 100) + (ehour % 100);
  return;
}

/************************************************************
 * function  : patname
 * purpose   : generates the pattern name given the pattern type number.
 ************************************************************/
char *patname(pattype)
     int pattype;
{
  switch(pattype)
    {
    case 0: return("EXACT");
    case 1: return("SET");
    case 2: return("REPEAT");
    case 3: return("STAR");
    default: return("ERROR");
    }
}
/************************************************************
 * function  : make_my_days
 * purpose   : generates a list of the days in the given month and year
 ************************************************************/

void make_my_days(month,year)
      int month, year;
     
{
  ELEMENT *temp;
  static int days_in_month[13] = {0,31,28,31,30,31,30,31,31,30,31,30,31};

  if (days_in->tu_time == star)
    {  int days, new_day;
      
      freelist(days_in);
      if (month == 2)
	{
	  if ( ( !(year % 4) && (year % 100) ) || 
	      ( !(year % 100) && !(year % 400) ) )
	    days = 29;
	  else days = 28;
	}
      else days = days_in_month[month];
      for (new_day = 1; new_day <= days; new_day++)
	{
	  temp = create_element();
	  temp->tu_time = new_day;
	  append(days_in,temp);
	}
     } 
}

/************************************************************
 * function  : process_year
 * purpose   : converts yearly data to one time list.
 ************************************************************/
int process_year()
{
  ELEMENT *year, *temp;
  unsigned long int temp_time, smin, emin;

#if DEBUG  
  printf("IN process_year\n");
#endif

  range(&smin,&emin);
  year = years_in;
  while (year = year->tu_next)
    {  
      temp_time = 1440*date2days(1,1,year->tu_time);
      if (!temp_time) 
	return;
      if (temp_time > emin) 
	return;
      if (smin <= temp_time) 
	{
	  temp = create_element();
	  temp->tu_time = temp_time;
	  append(timelist,temp);
	}
    }
  return(1);
}

/************************************************************
 * function  : process_month
 * purpose   : converts yearly data to one time list.
 ************************************************************/
int process_month()
{ ELEMENT *year, *month, *temp;
  unsigned long int temp_time, smin, emin;

#if DEBUG  
  printf("IN process_month\n");
#endif
  
  range(&smin,&emin);
  year = years_in;
  while (year = year->tu_next)
    {  
      month = months_in;
      while (month = month->tu_next)
	{ 
	  temp_time = 1440*date2days(1,month->tu_time,year->tu_time);
	  if (!temp_time) 
	    return;
	  if (temp_time > emin) 
	    return;
	  if (smin <= temp_time) 
	    {
	      temp = create_element();
	      temp->tu_time = temp_time;
	      append(timelist,temp);
	    }
	}
    }
  return(1);
}

/************************************************************
 * function  : process_day
 * purpose   : converts yearly data to one time list.
 ************************************************************/
int process_day()
{
  ELEMENT *year, *month, *day, *temp;
  unsigned long int temp_time, smin, emin;

#if DEBUG  
  printf("IN process_day\n");
#endif
  
  range(&smin,&emin);
  year = years_in;
  while (year = year->tu_next)
    {  
      month = months_in;
      while (month = month->tu_next)
	{ 
	  make_my_days(month->tu_time,year->tu_time);
	  day = days_in;
	  while (day = day->tu_next)
	    { 
	      /* Have to add the sixty because daily information is for 0100 */
	      temp_time = 1440*date2days(day->tu_time,month->tu_time,year->tu_time)
		+ 60;
	      if (!temp_time) 
		return;
	      if (temp_time > emin) 
		return;
	      if (smin <= temp_time) 
		{
		  temp = create_element();
		  temp->tu_time = temp_time;
		  append(timelist,temp);
		}
	    }
	}
    }
  return(1);
}

/************************************************************
 * function  : process_hour
 * purpose   : converts yearly data to one time list.
 ************************************************************/
int process_hour()
{
  ELEMENT *year, *month, *day, *hour, *temp;
  unsigned long int temp_time, smin, emin;
  
#if DEBUG  
  printf("IN process_hour\n");
#endif

  range(&smin,&emin);
  year = years_in;
  while (year = year->tu_next)
    {  
      month = months_in;
      while (month = month->tu_next)
	{ 
	  make_my_days(month->tu_time,year->tu_time);
	  day = days_in;
	  while (day = day->tu_next)
	    { 
	      hour = hours_in;
	      while (hour = hour->tu_next)
		{
		  temp_time = 1440*date2days(day->tu_time,month->tu_time,year->tu_time) +  60*(hour->tu_time/100) + (hour->tu_time % 100);
		  if (!temp_time) 
		    return;
		  if (temp_time > emin) 
		    return;
		  if (smin <= temp_time) 
		    {
		      temp = create_element();
		      temp->tu_time = temp_time;
		      append(timelist,temp);
		    }
		}
	    }
	}
    }
  return(1);
}

/************************************************************
 * function  : genyears
 * purpose   : generates the exact years from the input pattern.
 ************************************************************/
void genyears (pattype)
     int pattype;
{
  ELEMENT *temp;

#if DEBUG  
  printf("In genyears with pattern type %s\n",patname(pattype));
#endif
  
  years_in->tu_time = pattype;
  
  if (pattype == star) 
    { int syear, eyear, year;
      
      freelist(years_in);
      syear = atoi(substr(sdate,6,9));
      eyear = atoi(substr(edate,6,9));
      for (year = syear; year <= eyear; year++)
	{
	  temp = create_element();
	  temp->tu_time = year;
	  append(years_in,temp);
	}
    }
  
  if ( (pattype == repeat) && (EVALUATE) )
    { 
      register unsigned long int temp_time;
      unsigned long int smin, emin;
      int day, mon, year, repeat_year;
      
      range(&smin,&emin);

      years_in = years_in->tu_next; 
      repeat_year = years_in->tu_time;
	
      day = atoi(substr(sdate,3,4));
      mon = atoi(substr(sdate,0,1));
      year = atoi(substr(sdate,6,9));
      
      for ( temp_time = smin; temp_time <= emin; )
	{
	  temp = create_element();
	  temp->tu_time = temp_time;
	  append(timelist,temp);
	  year += repeat_year;
	  temp_time = 1440*date2days(day, mon, year) + (smin % 1440);
	  if (!temp_time)
	    return;
	}
    }
  /* all we saw were sets, exact values, and stars. need to process */
  else if (EVALUATE)
    {
      if (pattype == set) 
	if (!(sortlist(years_in)))
	  return;
      
      switch (estep(step))
	{
	case irregular:
	  process_hour();
	  break;
	case minutes:
	  process_hour();
	  break;
	case hours:
	  process_hour();
	  break;
	case days:
	  process_day();
	  break;
	case weeks:
	  process_day();
	  break;
	case months:
	  process_month();
	  break;
	case years:
	  process_year();
	  break;
	  default:
	  process_hour(); 
	  break;
	}
    }
  EVALUATE = 1;

  if (!timelist->tu_next)
    return;

#if DEBUG  
  prtlist(timelist);
#endif

  return;
}

/************************************************************
 * function  : genmonths
 * purpose   : generates the exact months from the input pattern.
 ************************************************************/
void genmonths (pattype)
     int pattype;
{
  ELEMENT *temp;
  
#if DEBUG  
  printf("In genmonths with pattern type %s\n",patname(pattype));
#endif

  months_in->tu_time = pattype;

  if (pattype == star) 
    { 
      int month;
      freelist(months_in);
      for (month = 1; month <= 12; month++)
      	{
	  temp = create_element();
	  temp->tu_time = month;
	  append(months_in,temp);
	}
    }
  
  if ( (pattype == repeat) && (EVALUATE) )
    { 
      register unsigned long int temp_time;
      unsigned long int smin, emin;
      int day, mon, year, repeat_mon;
      
      EVALUATE = 0;
      range(&smin,&emin);

      months_in = months_in->tu_next; 
      repeat_mon = months_in->tu_time;
      
      day = atoi(substr(sdate,3,4));
      mon = atoi(substr(sdate,0,1));
      year = atoi(substr(sdate,6,9));
      
      for ( temp_time = smin; temp_time <= emin; )
	{
	  temp = create_element();
	  temp->tu_time = temp_time;
	  append(timelist,temp);
	  mon += repeat_mon;
	  year += (mon / 13);
	  if (!(mon %= 12)) mon = 12;
	  temp_time = 1440*date2days(day, mon, year) + (smin % 1440);
	  if (!temp_time)
	    return;
	}
    }
  
  if (pattype == set) 
    if (!(sortlist(months_in)))
      return;

  return;
}

/************************************************************
 * function  : gendays
 * purpose   : generates the exact days for the input pattern.
 ************************************************************/
void gendays (pattype)
     int pattype;
{
  ELEMENT *temp;

#if DEBUG  
  printf("In gendays with pattern type %s\n",patname(pattype));
#endif
  
  days_in->tu_time = pattype;

  if (pattype == star)
    freelist(days_in);
  else if ( (pattype == repeat) && (EVALUATE) )
    { 
      register unsigned long int temp_time, timeinc;
      unsigned long int smin, emin;
      
      EVALUATE = 0;
      range(&smin,&emin);

      days_in = days_in->tu_next;
      timeinc = 1440*(days_in->tu_time);
      
      for (temp_time = smin; temp_time <= emin; temp_time += timeinc)
	{
	  temp = create_element();
	  temp->tu_time = temp_time;
	  append(timelist,temp);
	}
    }

  if (pattype == set) 
    if (!(sortlist(days_in)))
      return;

  return;
}

/************************************************************
 * function  : genhours
 * purpose   : generates the exact hours for the input pattern.
 ************************************************************/
void genhours(pattype)
     int pattype;
{ 
  ELEMENT *temp;
  
#if DEBUG  
  printf("In genhours with pattern type %s\n",patname(pattype));
#endif
 
  hours_in->tu_time = pattype;
  if (pattype == star)
    {
      freelist(hours_in);
      switch (estep(step))
	{ int mins, hrs, time;
	case minutes:
	  for (hrs = 0; hrs <= 2400; hrs += 100)
	    for (mins = 00; mins <= 45; mins+= 15)
	      {
		time = hrs + mins;
		if ( (0 < time) && (time <= 2400) )
		  {
		    temp = create_element();
		    temp->tu_time = time;
		    append(hours_in,temp);
		  }
	      }
	  break;
	case hours:
	  for (mins = 100; mins <= 2400; mins+= 100)
	    {
	      temp = create_element();
	      temp->tu_time = mins;
	      append(hours_in,temp);
	    }
	  break;
	}
    }
  
  if ( (pattype == repeat) && (EVALUATE) )
    { 
      register unsigned long int temp_time, timeinc;
      unsigned long int smin, emin;
      
      EVALUATE = 0;
      range(&smin,&emin);
      
      hours_in = hours_in->tu_next; 
      timeinc = 60*(hours_in->tu_time / 100) + (hours_in->tu_time % 100);
       
      for (temp_time = smin; temp_time <= emin; temp_time += timeinc)
	{
	  temp = create_element();
	  temp->tu_time = temp_time;
	  append(timelist,temp);
	}
    }

    if (pattype == set) 
      if (!(sortlist(hours_in)))
	return;

  return;
}

/************************************************************
 * function  : store_element;
 * purpose   : stores the element seen by the parser
 ************************************************************/
int store_element(element)
     char *element;
     
{ ELEMENT *temp;
  int iday, imonth;

  temp = create_element();

  switch (pos - NIPAT)
    {
    case 1: 
      temp->tu_time = atoi(element); 
      if  ( (temp->tu_time < 0) || (temp->tu_time > 2400))
	{
	  fprintf(stderr, "Illegal time unit: %d\n",temp->tu_time);
	  skipline();
	  return(0);
	}
      append(hours_in,temp);
      return(1);
    case 2:
      iday = day2value(element);
      if (!iday)
	return(0);
      temp->tu_time = iday;
      append(days_in,temp);
      return(1);
    case 3:
      imonth = month2value(element);
      if (!imonth) 
	return(0);
      temp->tu_time = imonth;
      append(months_in,temp);
      return(1);
    case 4:
      temp->tu_time = atoi(element);
      append(years_in,temp);
      return(1);
    }
}


# line 1087 "ascii2dss.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
 {
  char ch;
  char *str;
} YYSTYPE;
# define STAR 257
# define STATIC 258
# define HEXACT 259
# define DEXACT 260
# define MEXACT 261
# define YEXACT 262
# define COMMENT 263

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 1263 "ascii2dss.y"



/*****************************************************************
 * function: yyerror
 * purpose : demanded by yacc for error message output
 ******************************************************************/
  yyerror(s) 
	char *s; 
{  
  char line[LINESIZE];
  
  if ( fgets( line, LINESIZE - 1, fp ) != NULL )
    { 
      fprintf(stderr, "SKIPPING LINE %d: %s\n",lineno, s);
      lineno++;
      pos = -1;
      EVALUATE = 1;
      freelist(timelist);
      freelist(hours_in);
      freelist(days_in);
      freelist(months_in);
      freelist(years_in);
      area[0] = '\0';
      data_type[0] = '\0';
      data_units[0] = '\0';
      study[0] = '\0';
      sdate[0] = '\0';
      stime[0] = '\0';
      edate[0] = '\0';
      etime[0] = '\0';
      value[0] = '\0';
      return;
     }
}


/************************************************************
 * function  : genpathname
 * purpose   : Generates a dss pathname from the raw record.
 ************************************************************/
void genpathname(path)
     char path[80];
{ int i = 0;

  /* create the path name */

  path[0] = NULL;

  strcat(path,"/");                      /* A-Part */
  strcat(path,str2upper(area));          
  strcat(path,"/");
  
  strcat(path,str2upper(location));      /* B-Part */
  strcat(path,"/");
  
  strcat(path,str2upper(data_type));      /* C-Part */
  strcat(path,"/");
                                          /* NO D-Part */
  strcat(path,"/");
  
  strcat(path,str2upper(step));           /* E-Part */
  strcat(path,"/");
  
  strcat(path,str2upper(study));          /* F-Part */
  strcat(path,"/");

  strcat(path,"\0");

  for (i = strlen(path); i <= 80; path[i++] = ' ')
    ;

  return;
}

/************************************************************
 * function  : gendate
 * purpose   : Generates the date-month-year string from the 
 *           :  integer absolute minutes since midnight 31-DEC-1899.
 ************************************************************/
char *gendate(abtime)
     int abtime;
{
  int abdays, year, days, date, i = 0, leaps;
  char *syear, *sdate, *smonth, *date_string;
  static int mdays[12] = {31,59,90,120,151,181,212,243,273,304,334,365};
  static char *months_of_year[12] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"};

  date_string = (char *)malloc(INSTRLEN*sizeof(char));

  /* Convert absoulte minutes to absolute days */
  abdays =  abtime / 1440;
  year = (abdays / 365);
  syear = int2string(1900 + year);

  /* The days left after computing the year */
  days = abdays % 365;

  /* Subtract the leap days to get the number of days since the beginning of year */
  leaps = (year - 1)/4 + (year - 1)/400 - (year - 1)/100;
  days = days - leaps;

  /* Account for current year being a leap year */
  if (days > 59)
    if ( ( !(year % 4) && (year % 100) ) || 
	( !(year % 100) && !(year % 400) ) )
      days -= 1;
  
  /* Compute the month */
  while (mdays[i] <= days)
    i++;
  
  if (days <= 31)
    date = days;
  else date = days - mdays[i - 1];
  
  sdate = int2string(date);
  smonth = months_of_year[i];

  strcat(date_string,sdate);
  strcat(date_string," ");
  strcat(date_string,smonth);
  strcat(date_string," ");
  strcat(date_string,syear);

  return(date_string);
}

/************************************************************
 * function  : gentime
 * purpose   : Generates military time from first time unit in
 *           :  the list.
 ************************************************************/
char *gentime(abtime)
     int abtime;
{ 
  int time, min, hour, stimelen;
  char *stime, *shift_stime;

  shift_stime = (char *)malloc(4*sizeof(char)); 

  time = abtime % 1440;
  hour =  time/60;
  min =  time % 60;
  time = 100*hour + min;
  stime = int2string(time);
  stimelen = strlen(stime);
  
  switch (stimelen)
    {
    case 0 : 
      strcat(shift_stime, "0000");
      break;
    case 1 : 
      strcat(shift_stime, "000");
      strcat(shift_stime, stime);
      break;
    case 2 : 
      strcat(shift_stime, "00");
      strcat(shift_stime, stime);
      break;
    case 3 : 
      strcat(shift_stime, "0");
      strcat(shift_stime, stime);
      break;
    case 4 :
      shift_stime = stime;
      break;
    default:
      yyerror("Illegal time unit");
      return(NULL);
    }

  return(shift_stime);
}


/************************************************************
 * function  : convert2dss_objects
 * purpose   : writes list to a dss database
 ************************************************************/
void convert2dss_objects(list)
     ELEMENT *list;
{ 
  extern wrt2dss_();
  
  char path[80],  *date, *time;
  float dataarray[35040];
  int timearray[35040];
  ELEMENT *l;
  int nvals = 0, i, isirregular;
  
#if DEBUG 
  printf("In convert2dss_objects\n");
#endif

  genpathname(path);

  date = gendate(list->tu_next->tu_time); 
  strcat(date,"\0");
  /* Fill end of  date with spaces */   
  for (i = strlen(date); i <= INSTRLEN; date[i++] = ' ')
    ;

  time = gentime(list->tu_next->tu_time);
  if (time == NULL)
    return;

  /* Create the data array */
  l = list;
  while (l = l->tu_next)
    {
      timearray[nvals] = l->tu_time;
      dataarray[nvals] = (float) atof(value);
      nvals++;
    }

   /* Fill end of  data_units with spaces */
  for (i = strlen(data_units); i <= INSTRLEN; data_units[i++] = ' ')
    ;
   /* Fill end of  data_type with spaces */   
  for (i = strlen(data_type); i <= INSTRLEN; data_type[i++] = ' ')
    ;
   /* Fill end of dssfile with spaces */   
  for (i = strlen(dssfile); i <= INSTRLEN; dssfile[i++] = ' ')
    ;
  
  /* Is data regular or irregular */
  if (strseq(substr(str2upper(step), 0, 2),"IR-"))
    isirregular = 1;
  else isirregular = 0;
  
    wrt2dss_(path,date,time,timearray,&nvals,dataarray,data_units,data_type,dssfile,&isirregular);
  return;
}
  
/*****************************************************************
 * function: yylex
 * purpose : demanded by yacc to generate input for yyparse.
 ******************************************************************/
yylex()
{ char chval='x';
  int i = 0, initial = 1;

  /* skip over some whitespace */
  while ((chval = fgetc(fp)) == ' ' || chval == '\t')
    if (initial)
      { pos++;
	initial = 0;
      }

  if (chval == EOF) 
    {
      return(0); 
    }

  if (chval == '\n')
    { 
      pos = -1;
      lineno++;
      EVALUATE = 1;
      freelist(timelist);
      freelist(hours_in);
      freelist(days_in);
      freelist(months_in);
      freelist(years_in);
      area[0] = '\0';
      data_type[0] = '\0';
      data_units[0] = '\0';
      study[0] = '\0';
      sdate[0] = '\0';
      stime[0] = '\0';
      edate[0] = '\0';
      etime[0] = '\0';
      value[0] = '\0';
      yylval.ch = chval;
      return(STATIC);
    }
	
  if ( (chval == '*') && ( ( (pos - NIPAT) > 0) && ((pos - NIPAT) < 5) ) )
    {
      yylval.ch = chval;
      return(STAR);
    }

  if (chval == COMMENT_SYMBOL)
    {
      yylval.ch = chval;
      return(COMMENT);
    }

  if ( (chval == '[') || (chval == ']') || (chval == '(') || (chval == ')') ||
      (chval == ',') )
    { 
      yylval.ch = chval;
      return(chval);
    }

  else
    { if ( (isalnum(chval)) || (chval == '/')  || (chval == '.') ||  (chval == '*') 
	  ||  (chval == '-') )
	instr[i++] = chval;
      while  ( isalnum( chval = fgetc(fp)) || (chval == '/')  || (chval == '.')
	       ||  (chval == '-')) 
	instr[i++] = chval;
      instr[i++] = '\0';
      /* since we took a non-alphanumeric, we better put it back */
      chval = ungetc(chval,fp);
      yylval.str = instr;
      switch (pos)
	{
	case -1: 
	  strcpy(area,instr);
	  pos++;
	  return(STATIC);
	case 0: 
	  strcpy(area,instr);
	  return(STATIC);
	case 1: 
	  strcpy(location,instr);
	  return(STATIC);
	case 2: 
	  strcpy(data_type,instr);
	  return(STATIC);
	case 3: 
	  strcpy(data_units,instr);
	  return(STATIC);
	case 4: 
	  strcpy(study,instr);
	  return(STATIC);
	case 5: 
	  strcpy(step,instr);
	  return(STATIC);
	case 6: 
	  strcpy(sdate,instr);
	  return(STATIC);
	case 7: 
	  strcpy(stime,instr);
	  return(STATIC);
	case 8: 
	  strcpy(edate,instr);
	  return(STATIC);
	case 9: 
	  strcpy(etime,instr);
	  return(STATIC);
	case 10: 
	  return(HEXACT);
	case 11:
	  return(DEXACT);
	case 12:
	  return(MEXACT);
	case 13:
	  return(YEXACT);
	case 14: 
	  strcpy(value,instr);
	  return(STATIC);
	case 15:
	  strcpy(dssfile,instr);
	  convert2dss_objects(timelist);
	  return(STATIC);
	default:
	  yyerror("%s is not valid for this field",instr);
	}
    }
}

/*****************************************************************
 * function: main
 * purpose : opens files and calls yyparse, the main parsing loop that
 *           calls yylex (used to recognize and tokenize the input)
 *           until yylex returns 0.
 ******************************************************************/
main(argc,argv)
     int argc;
     char *argv[];
{ int file, files;
  char chval;

/* Initialize some variables */  
  timelist = create_element();
  hours_in = create_element();
  days_in = create_element();
  months_in = create_element();
  years_in = create_element();

  for (files = 1; files < argc; files++)
    /* check to see if we can open a file for reading */
    if ((fp = fopen(argv[files],"r")) == NULL)
      fprintf(stderr,"can't open %s\n",argv[files]);
  /* parse the file, if it's open */
    else yyparse();
  printf("closing file %s\n",argv[files - 1]);
  fclose(fp);
  return(0);
}
yytabelem yyexca[] ={
-1, 0,
	0, 1,
	257, 1,
	258, 1,
	259, 1,
	260, 1,
	261, 1,
	262, 1,
	263, 1,
	32, 1,
	9, 1,
	10, 1,
	40, 1,
	91, 1,
	-2, 0,
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 30
# define YYLAST 255
yytabelem yyact[]={

     4,     5,    23,    24,    25,    26,    37,    36,    35,    34,
     2,    33,    32,    31,    30,    29,    28,    18,    22,    21,
    20,    19,    15,     3,    14,     6,     1,     0,     0,     0,
     0,    16,     0,     0,     0,    27,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    17,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     9,     8,
    10,    11,    12,    13,     7 };
yytabelem yypact[]={

  -246,    -9,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -257,  -257,   -25,   -29,
   -30,   -31,   -32,-10000000,-10000000,-10000000,-10000000,   -82,-10000000,  -250,
  -252,  -254,  -256,-10000000,-10000000,-10000000,-10000000,-10000000 };
yytabelem yypgo[]={

     0,    26,    25,    24,    22,    17,    21,    20,    19,    18 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     1,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     3,     4,     5,     5,
     5,     5,     6,     6,     7,     7,     8,     8,     9,     9 };
yytabelem yyr2[]={

     0,     0,     4,     4,     4,     4,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     6,     6,     2,     2,
     2,     2,     3,     7,     3,     7,     3,     7,     3,     7 };
yytabelem yychk[]={

-10000000,    -1,   256,    32,     9,    10,    -2,   263,   258,   257,
   259,   260,   261,   262,    -3,    -4,    40,    91,    -5,    -6,
    -7,    -8,    -9,   259,   260,   261,   262,    -5,    41,    44,
    44,    44,    44,    93,   259,   260,   261,   262 };
yytabelem yydef[]={

    -2,    -2,     6,     2,     3,     4,     5,     7,     8,     9,
    10,    11,    12,    13,    14,    15,     0,     0,     0,    18,
    19,    20,    21,    22,    24,    26,    28,     0,    16,     0,
     0,     0,     0,    17,    23,    25,    27,    29 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"STAR",	257,
	"STATIC",	258,
	"HEXACT",	259,
	"DEXACT",	260,
	"MEXACT",	261,
	"YEXACT",	262,
	"COMMENT",	263,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"item : /* empty */",
	"item : item ' '",
	"item : item '\t'",
	"item : item '\n'",
	"item : item pattern",
	"item : error",
	"pattern : COMMENT",
	"pattern : STATIC",
	"pattern : STAR",
	"pattern : HEXACT",
	"pattern : DEXACT",
	"pattern : MEXACT",
	"pattern : YEXACT",
	"pattern : set",
	"pattern : repeat",
	"set : '(' list ')'",
	"repeat : '[' list ']'",
	"list : hlist",
	"list : dlist",
	"list : mlist",
	"list : ylist",
	"hlist : HEXACT",
	"hlist : hlist ',' HEXACT",
	"dlist : DEXACT",
	"dlist : dlist ',' DEXACT",
	"mlist : MEXACT",
	"mlist : mlist ',' MEXACT",
	"ylist : YEXACT",
	"ylist : ylist ',' YEXACT",
};
#endif /* YYDEBUG */
#if !defined(lint) && !defined(__cplusplus)
static  char __yaccpar_sccsid1[] = "@(#) 9/3/92 yaccpar 6.11 Copyr 1991 Sun Micro";
#endif

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
			skip_init:
				yynerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 6:
# line 1117 "ascii2dss.y"
{ yyerrok;
                    yyclearin;
                  } break;
case 7:
# line 1123 "ascii2dss.y"
{ skipline(); } break;
case 8:
# line 1124 "ascii2dss.y"
{ printf(""); } break;
case 9:
# line 1125 "ascii2dss.y"
{switch (pos - NIPAT)
                   {
		   case 1:
		     genhours(star); 
		     break;
		   case 2: 
		     gendays(star);
		     break;
		   case 3: 
		     genmonths(star);
		     break;
		   case 4: 
		     genyears(star); 
		     break;
		   }
                } break;
case 10:
# line 1142 "ascii2dss.y"
{ 
                  if (store_element(yylval.str))
                   genhours(exact); 
                } break;
case 11:
# line 1146 "ascii2dss.y"
{ 
                  if (store_element(yylval.str))
                   gendays(exact);
                } break;
case 12:
# line 1150 "ascii2dss.y"
{ 
                  if (store_element(yylval.str))
		   genmonths(exact);
                } break;
case 13:
# line 1154 "ascii2dss.y"
{ 
                  if (store_element(yylval.str))
		  genyears(exact); 
                } break;
case 14:
# line 1159 "ascii2dss.y"
{  switch (pos - NIPAT)
                   {
		   case 1: 
		     genhours(set); 
		     break;
		   case 2:
		     gendays(set);  	
		     break;
		   case 3:
		     genmonths(set);
		     break;
		   case 4:
		     genyears(set);
		     break;
		   }
                } break;
case 15:
# line 1175 "ascii2dss.y"
{ switch (pos - NIPAT)
                   {
		   case 1: 
		     if (nelements(hours_in) > 1)
		       yyerror("Only one value allowed per repeat pattern");
		     else genhours(repeat); 
		     break;
		   case 2:
		     if (nelements(days_in) > 1)
		       yyerror("Only one value allowed per repeat pattern");
		     else gendays(repeat); 
		     break;
		   case 3:
		     if (nelements(months_in) > 1)
		       yyerror("Only one value allowed per repeat pattern");
		     else genmonths(repeat);
		     break;
		   case 4:
		     if (nelements(years_in) > 1)
		        yyerror("Only one value allowed per repeat pattern");
		     else genyears(repeat);
		     break;
		   }
                } break;
case 22:
# line 1233 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 23:
# line 1234 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 24:
# line 1242 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 25:
# line 1243 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 26:
# line 1251 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 27:
# line 1252 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 28:
# line 1260 "ascii2dss.y"
{ store_element(yylval.str); } break;
case 29:
# line 1261 "ascii2dss.y"
{ store_element(yylval.str); } break;
	}
	goto yystack;		/* reset registers in driver code */
}

