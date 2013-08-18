/***********************************************************************
     
                              COPYRIGHT
     
     The following is a notice of limited availability of the code and 
     Government license and disclaimer which must be included in the 
     prologue of the code and in all source listings of the code.
     
     Copyright notice
       (c) 1977  University of Chicago
     
     Permission is hereby granted to use, reproduce, prepare 
     derivative works, and to redistribute to others at no charge.  If 
     you distribute a copy or copies of the Software, or you modify a 
     copy or copies of the Software or any portion of it, thus forming 
     a work based on the Software and make and/or distribute copies of 
     such work, you must meet the following conditions:
     
          a) If you make a copy of the Software (modified or verbatim) 
             it must include the copyright notice and Government       
             license and disclaimer.
     
          b) You must cause the modified Software to carry prominent   
             notices stating that you changed specified portions of    
             the Software.
     
     This software was authored by:
     
     Argonne National Laboratory
     J. Michalakes: (630) 252-6646; email: michalak@mcs.anl.gov
     Mathematics and Computer Science Division
     Argonne National Laboratory, Argonne, IL  60439
     
     ARGONNE NATIONAL LABORATORY (ANL), WITH FACILITIES IN THE STATES 
     OF ILLINOIS AND IDAHO, IS OWNED BY THE UNITED STATES GOVERNMENT, 
     AND OPERATED BY THE UNIVERSITY OF CHICAGO UNDER PROVISION OF A 
     CONTRACT WITH THE DEPARTMENT OF ENERGY.
     
                      GOVERNMENT LICENSE AND DISCLAIMER
     
     This computer code material was prepared, in part, as an account 
     of work sponsored by an agency of the United States Government.
     The Government is granted for itself and others acting on its 
     behalf a paid-up, nonexclusive, irrevocable worldwide license in 
     this data to reproduce, prepare derivative works, distribute 
     copies to the public, perform publicly and display publicly, and 
     to permit others to do so.  NEITHER THE UNITED STATES GOVERNMENT 
     NOR ANY AGENCY THEREOF, NOR THE UNIVERSITY OF CHICAGO, NOR ANY OF 
     THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
     ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
     COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, 
     PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
     NOT INFRINGE PRIVATELY OWNED RIGHTS.

***************************************************************************/
#include <stdio.h>
#include "n32.h"
#include "sym.h"
#include "dm.h"
#include "twst.h"

static int in_id_ref = 0 ;
static int doing_lhs = 0 ;

void * firsttok(), *my_malloc(), *malloc(), *strip_nl(), *strip_lead(), *index(), *newTokNode()  ;

char* make_upper(s)
char *s ;
{
    char *p ;
    for ( p = s ; *p ; p++ )
    {
      if (( *p >= 'a' ) && ( *p <= 'z' )) *p = ( *p - 'a' + 'A' ) ;
    }
    return(s) ;
}

/* 970910 */
collapse_idrefs( node ) 
  NodePtr node ;
{
  int d ;
  NodePtr dex, elder, paren ;
  sym_nodeptr id ;

  if (node == NULL)  return ;
  if ( MINORKIND(node) == idrefK )
  {
    if ( getclass(node) == ARRAY )
    {
      if ((id=sym_get(TOKSTRING(CHILD(node,0)))) != NULL )
      {
        if ((paren = CHILD(node,1)) != NULL)
        {

          for ( d=0 , dex=CHILD(paren,1) ; dex!=NULL; dex=SIBLING(dex) )
          {
            if ( MINORKIND(dex) == TCOMMA ) continue ;
            if (!strcmp( id->dimname[d],"REMOVED" ))
            {
                  /* snip it out of the list */
                  if (elder=ELDER(dex))
                    SIBLING(elder) = SIBLING(dex) ;
                  else /* must be number one */
                  {
                    CHILD(PARENT(dex),1) = SIBLING(dex) ;
                    if (SIBLING(dex))
                      ELDER(SIBLING(dex)) = NULL ;
                  }
            }
            d++ ;
          }
          /* get rid of extra commas */
          for ( dex=CHILD(paren,1) ; dex!=NULL; dex=SIBLING(dex) )
          {
            if ( MINORKIND(dex) == TCOMMA )
            {
              if (elder=ELDER(dex))
              {
                if (SIBLING(dex))
                {
                  if (MINORKIND(SIBLING(dex)) == TCOMMA)
                  {
                    SIBLING(elder) = SIBLING(dex) ;
                  }
                }
                else  /* trailing */
                {
                  SIBLING(elder) = NULL ;
                }
              }
              else /* must be number one */
              {
                CHILD(PARENT(dex),1) = SIBLING(dex) ;
                if (SIBLING(dex))
                  ELDER(SIBLING(dex)) = NULL ;
              }
            }
          }
          /* get rid of () thingies */
          if ((dex=CHILD(paren,1)) == NULL)
          {
            CHILD(node,1) = NULL ;
          }
        }
      }
    }
  }
}

handle_executable( node )
  NodePtr node ;
{
  sym_nodeptr x ;
  loop_t * LOOP ;
  array_t * AR ;
  NodePtr p, firstt, sib ;
  int var_child, body_child, range_child, d ;
  char loopbeginline[1024] ;

  if (node == NULL)  return ;

  if (p = (NodePtr) firsttok(node))
    statement_line_number = atoi(TOKBEGIN(p)) ;
  if (statement_line_number == 0)
    statement_line_number = atoi(TOKBEGIN(p)) ;
  if (statement_line_number == 0)
    statement_line_number = last_statement_line_number ; /* last resort  -- this can happen
                                                            for artificial nodes, like enddo
                                                            nodes that were put in to change
                                                            labeled doK blocks */

  last_statement_line_number = statement_line_number ;

  switch (MINORKIND(node))
  {
  case gotoK :
    /* check for the case we don't handle: where a goto references
       a do loop label. */
    if ((x=sym_get(TOKSTRING(CHILD(node,2)))) != NULL)
    {

      /* we're going to just have to allow this; it's quite
         prevalent in MM5 */
#if 0
      fprintf(stderr,"DM Error line %s: GOTO end of do loop label: %s\n",
             TOKBEGIN(CHILD(node,2)),
             TOKSTRING(CHILD(node,2))) ;
#endif
    }
    break ;
  case doenddoK :
    var_child = 3 ;             /* which child of node is loop var */
    body_child = 6 ;            /* which child of node is loop body */
    range_child = 5 ;           /* which child of node has ranges */
    /* FALL THROUGH */
  case doK :
    if ( MINORKIND(node) == doK )
      { var_child = 4 ; body_child = 7 ; range_child = 6 ;
        /* store label reference in symbol table */
        x = sym_add(TOKSTRING(CHILD(node,2))) ;
      }
    
    loop_level++ ;
    LOOP = (loop_t *)my_malloc(sizeof(loop_t));
    LOOP->ARLIST = NULL;
    LOOP->LI_IS = 0 ;
    LOOP->loop = node ;
    LOOP->indentation = atoi(TOKCOLS(CHILD(node,1)))-1 ; /* column of DO */
    strcpy(LOOP->LI,TOKSTRING(CHILD(node,var_child))) ;
    LOOP->next = LOOPLIST ;  /* push loop onto stack */
    LOOPLIST = LOOP ;

    /* mark the loop variable as assigned -- this terminates the set/def
       chains that will be checked later for dependencies on loop vars */
    x = sym_add(LOOP->LI) ;
    node->user_def = (NodePtr)x->assigned ;
    x->assigned = (unsigned long)node ;
    x->thisif = (unsigned long)thisif ;
    x->iflev = iflev ;

if ( sw_write_db ) {
  sprintf(loopbeginline,"stmtline %d loopvar %s",statement_line_number,LOOP->LI) ;
}

    walk_statements1( CHILD(node,body_child),handle_executable,
                      loop_level,loop_level ) ;
    /* make global, the  bare (non index) instances of the loop variable */
    LOOP = LOOPLIST ;
    switch ( LOOP->LI_IS )
    {
    case MDIM :
if ( sw_write_db ) {
  fprintf(db_file,"LOOPBEGIN: M %s\n",loopbeginline) ;
  fprintf(db_file,"LOOPEND: M stmtline %d loopvar %s\n",statement_line_number,LOOP->LI) ;
}
      p = (NodePtr)firsttok(node) ;
      surgery_add_loop_begin( MDIM, LOOP, node, range_child, body_child ) ;
      li_loc2glob( CHILD(node,body_child), LOOP->LI, MDIM ) ;
      break ;
    case NDIM :
if ( sw_write_db ) {
  fprintf(db_file,"LOOPBEGIN: N %s\n",loopbeginline) ;
  fprintf(db_file,"LOOPEND: N stmtline %d loopvar %s\n", statement_line_number,LOOP->LI) ;
}
      p = (NodePtr)firsttok(node) ;
      surgery_add_loop_begin( NDIM, LOOP, node, range_child, body_child ) ;
      li_loc2glob( CHILD(node,body_child), LOOP->LI, NDIM ) ;
      break ;
    default :
if ( sw_write_db ) {
  fprintf(db_file,"LOOPBEGIN: ? %s\n",loopbeginline) ;
  fprintf(db_file,"LOOPEND: ? stmtline %d loopvar %s\n", statement_line_number,LOOP->LI) ;
}
      if ( LOOP->LI_IS == (MDIM | NDIM) )
        { NodePtr frstt ; int linex ;
          linex = -1 ;
          if (( frstt = (NodePtr)firsttok(node)) != NULL ) linex=atoi(TOKBEGIN(frstt)) ;

         fprintf(stderr,"line %d: %s Loop over both dimensions. %d\n",linex,LOOP->LI,LOOP->LI_IS) ;
         exit(2);}
      /* this removes the statement label and puts an enddo on the loop */
      if ( MINORKIND(node) == doK )
      {
        surgery_add_loop_begin( 0, LOOP, node, range_child, body_child ) ;
      }
      break ;
    }
    LOOPLIST = LOOPLIST->next ;
    loop_level-- ;
    break ;


  case endK:

    LOOP = LOOPLIST ;
    /* handle references to arrays in the loop that are over
       the classified dimension but that are indexed by an expression
       that is not a function of LOOP->LI . */

    /* reverse the lst so traversal is in correct order of occurrence
       in file */

    { array_t * Z, * TEMPLIST ;
      TEMPLIST = NULL ;
      for ( AR = LOOP->ARLIST, Z = AR ; AR != NULL ;  )
      {
        Z = AR->next ;
        AR->next = TEMPLIST ;
        TEMPLIST = AR ;
        AR = Z ;
      }
      LOOP->ARLIST = TEMPLIST ;
    }

    for ( AR = LOOP->ARLIST ; AR != NULL ; AR  = AR->next )
    {
      for ( d = 0 ; d < AR->ndim ; d++ )
      {
        if ( AR->SYM->dims[d] != AR->Index_IS[d] )
        {
          if      (AR->SYM->dims[d] == MDIM)
          {
            surgery_addset_constants( AR, d, MDIM, body_child ) ;
          }
          else if (AR->SYM->dims[d] == NDIM )
          {
            surgery_addset_constants( AR, d, NDIM, body_child ) ;
          }
        }
      }
    }
#if 1
    /* make global, the  bare (non index) instances of the loop variable */
    LOOP = LOOPLIST ;
    switch ( LOOP->LI_IS )
    {
    case MDIM :
      li_loc2glob( first_statement, LOOP->LI, MDIM ) ;  /* 970225 */
      break ;
    case NDIM :
      li_loc2glob( first_statement, LOOP->LI, NDIM ) ;  /* 970225 */
      break ;
    default :
      break ;
    }
#endif
    cleanup(LOOP) ;
    break ;

  /**********************************************************************
     The rest of these need to be handled because descent into loop
     bodies is cut short by the last argument of zero in the call to
     walk_statements in the file dm.c */
  case iflogicalK :
    node->user_def = (NodePtr) thisif ;
    thisif = node ;
    iflev++ ;
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    walk_statements1( CHILD(node,5),handle_executable,loop_level,loop_level ) ;
    iflev-- ;
    thisif = node->user_def ;
    break ;
  case ifthenelseK :
    node->user_def = (NodePtr) thisif ;
    thisif = node ;
    iflev++ ;
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    walk_statements1( CHILD(node,6),handle_executable,loop_level,loop_level ) ;
    walk_statements1( CHILD(node,8),handle_executable,loop_level,loop_level ) ;
    iflev-- ;
    thisif = node->user_def ;
    break ;
  case ifthenelseifK :
    node->user_def = (NodePtr) thisif ;
    thisif = node ;
    iflev++ ;
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    walk_statements1( CHILD(node,6),handle_executable,loop_level,loop_level ) ;
    walk_statements1( CHILD(node,7),handle_executable,loop_level,loop_level ) ;
    iflev-- ;
    thisif = node->user_def ;
    break ;
  case dowhileK :
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    walk_statements1( CHILD(node,5),handle_executable,loop_level,loop_level ) ;
    break ;
  case readK :
  case writeK :
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    break ;
  case stopK :
    if ( sw_change_stops )
    {
      char pp[1024] ;
      MINORKIND(node) = 9001 ;   /* morph the node */
      node->user_def = (NodePtr) my_malloc( 1024 ) ;
      if ( CHILD(node,2) != NULL )
      {
        int pplen, chlen ;
        char *p, *q ;
        strcpy( pp, TOKSTRING(CHILD(node,2)) ) ;
        chlen = strlen(sw_change_stops_string) ;
        for ( p = pp, q = pp ; *q ; )   /* remove quotes */
        {
          while ( *q == '\'' || *q == '"' ) q++ ;
          *p++ = *q++ ;
        } *p = '\0' ;
        pplen = strlen(pp) ;
        if ( pplen+chlen > 50 ) pp[50-chlen] = '\0' ;  /* wtch length */
        if ( MINORKIND(CHILD(node,2)) == TICON  )
        {
          sprintf((char *)node->user_def,"      CALL %s('%s') \n",sw_change_stops_string,pp) ;
        }
        if ( MINORKIND(CHILD(node,2)) == TSTRING )
        {
          sprintf((char *)node->user_def,"      CALL %s('%s') \n",sw_change_stops_string,pp) ;
        }
      }
      else
      {
        sprintf((char *)node->user_def,"      CALL %s('BARE STOP %s')\n",sw_change_stops_string,
               modulename ) ;
      }
    }
    break ;
  case assignK :
    /* we're interested in where int scalars, which may be used
       for indexes, are assigned in the program (we don't look
       at arrays because it's not likely they'd be used as indeces,
       plus we wouldn't necessarily know which element. */
    {
      if ( getclass(CHILD(node,1)) == VARIABLE )
      {
        x = sym_add(TOKSTRING(CHILD(CHILD(node,1),0))) ;
        node->user_def = (NodePtr)x->assigned ;
        x->assigned = (unsigned long)node ;
        x->thisif = (unsigned long) thisif ;
        x->iflev = iflev ;
      }
    }
    sib = SIBLING(node) ;
    SIBLING(node) = NULL ;
    doing_lhs = 1 ;
    walk_depth( CHILD(node,1), handle_refs, 0 ) ;
    doing_lhs = 0 ;
    walk_depth( CHILD(node,3), handle_refs, 0 ) ;
    SIBLING(node) = sib ;
    break ;
  case callK :
    if ( sw_addargs || sw_addgenericargs )
    {
      NodePtr q, p, prev_q ;
      sym_nodeptr x ;

      char tmp[256] ;
      for ( q = CHILD(node,4) ; q != NULL ; prev_q = q , q = SIBLING(q) )
      {
        if ( MINORKIND(q) == TCOMMA ) continue ;
        if ( MINORKIND(q) == idrefK )
        {
          if ((x = sym_get( TOKSTRING(CHILD(q,0)) )) != NULL )
          {
            /* 
               This handles instances where the argument is one of
               the identifiers that have been listed using -mdim
               or -mdim directives.  It inserts a macro around the
               argument to allow adding arguments.
            */
            if ( x->dim == MDIM || x->dim == NDIM )
            {
              p = CHILD(q,0) ;
              MINORKIND(p) = 9009 ;    /* morph the node */
              sprintf(tmp,"D_ARG_%c(%s)",(x->dim==MDIM)?'M':'N',
                                         TOKSTRING(p)) ;
              p->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
              strcpy(p->user_def,tmp) ;
            }
            /*
               We want to be able to handle cases where a decomposed
               array is passed to a subroutine, but with index arguments.
               If the index argument to a decomposed dimension is '1', and
               all the index arguments to the left are also '1', then the
               intention on the part of the user is assumed to be that they
               want a pointer to the first element of the array.  We need
               to adjust the arguments so that they point to the first
               element in the local array, not just the first element
               of the partition.  Do this by inserting a macro around
               the argument.
            */
            {
              NodePtr paren, arg, node ;
              int keepgoing, i;

              keepgoing = 1 ;
              i = 0 ;
              if (( paren = CHILD(q,1)) != NULL )
              {
                for (arg = CHILD(paren,1);
                     arg != NULL && keepgoing ;
                     arg = SIBLING(arg))
                {
                  if ( MINORKIND(arg) == TCOMMA ) continue ;
                  if ( MINORKIND(arg) == TICON )
                  {
                    if (!strcmp(TOKSTRING(arg),"1"))
                    {
                      if (x->dims[i] == MDIM || x->dims[i] == NDIM )
                      {
                        MINORKIND(arg) = 9009 ;  /* morph the node */
                        sprintf(tmp,"D_BASEDEX_%c(%s)",(x->dims[i]==MDIM)?'M':'N',
                                         TOKSTRING(arg)) ;
                        arg->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
                        strcpy(arg->user_def,tmp) ;
                      }
                    }
                    else
                    {
                      keepgoing = 0 ;
                    }
                  }
                  else
                  {
                    keepgoing = 0 ;
                  }
                  i++ ;
                }
              }
            }
          }
        }
      }
      if ( sw_addgenericargs && prev_q != NULL )
      {
        q = prev_q ;
        if ( SIBLING(q) == NULL )
        {
          NodePtr NewNode ;
          TokInfoPtr p ;
          p = (TokInfoPtr)malloc(sizeof(*p)) ;
          NewNode = (NodePtr) newTokNode( p ) ;
          SIBLING(NewNode) = NULL ;
          ELDER(NewNode) = q ;
          MINORKIND(NewNode) = 9009 ;    /* morph the node */
          sprintf(tmp,",D_ARG_G") ;
          NewNode->user_def = (NodePtr) malloc(strlen(tmp)+1) ;
          strcpy(NewNode->user_def,tmp) ;
          SIBLING(q) = NewNode ;
        }
      }
    }
    /* FALL THROUGH */
  default :
    sib = SIBLING(node) ;
    SIBLING(node) = NULL ;
    walk_depth( node, handle_refs, 0 ) ;
    SIBLING(node) = sib ;
    break ;
  }
}

handle_refs( node )
  NodePtr node ;
{
  NodePtr paren, dex, elder ;
  array_t * AR, * p ;
  loop_t * LOOP ;
  NodePtr firstt, dummy[DEPENDS_MAXRET] ;
  int nret ;
  int d, linex ;
char outstr[1024], tmpstr[1024] ;

  if (node == NULL) return ;
  if ( MINORKIND(node) != idrefK ) return ;
  if (( firstt = (NodePtr) firsttok(node)) != NULL )
    linex = atoi(TOKBEGIN(firstt)) ;
  switch (getclass(node))
  {
  case ARRAY:
    AR = (array_t *)my_malloc(sizeof(array_t)) ;
    if ((AR->SYM=sym_get(TOKSTRING(CHILD(node,0)))) == NULL )
    { 
        if ( ! sw_quiet )
        {
        fprintf(stderr,"Advisory: Line %s: No declaration for %s.\n",
                TOKBEGIN(CHILD(node,0)),TOKSTRING(CHILD(node,0))) ;
        }
        return ;
    }
    AR->Ref = node ;
    AR->iflev = iflev ;
    if ((paren = CHILD(node,1)) != NULL)
    {
      for ( AR->ndim=0, dex = CHILD(paren,1) ; dex != NULL; dex = SIBLING(dex) )
        {  if (MINORKIND(dex) == TCOMMA) continue ; AR->ndim++ ; }
outstr[0] = '\0' ;
      for ( LOOP = LOOPLIST ; LOOP != NULL ; LOOP=LOOP->next )
      {
        for ( d=0 , dex=CHILD(paren,1) ; dex!=NULL; dex=SIBLING(dex) )
        {
          if (MINORKIND(dex) == TCOMMA) continue ;
          AR->Index[d] = dex ;
          if ( depends_on(dex, LOOP->LI, linex, dummy, 0))
          {
if (sw_write_db &&((AR->SYM->dims[d] == MDIM)||(AR->SYM->dims[d] == NDIM)))
{
  int ofst ;
  int ifull ;
  char * fullorpart ;
  int i ;
  ofst = offset(dex, LOOP->LI, linex) ;
  if ( 1 )  /* doing_lhs || ofst != 0 ) */
  {
     ifull = 0 ;
     for ( i = 0 ; i < AR->ndim ; i++ )
     {
       if ( AR->SYM->dims[i] == MDIM ) ifull |= 1 ;
       if ( AR->SYM->dims[i] == NDIM ) ifull |= 2 ;
     }
     switch ( ifull )
     {
     case 1: fullorpart = "partM" ; break ;
     case 2: fullorpart = "partN" ; break ;
     case 3: fullorpart = "full" ; break ;
     default : fullorpart ="?" ; break ;
     }
     if ( ifull > 0 )
     {
     if ( strlen(outstr) == 0 )
     {
       sprintf(outstr,"%s: %c stmtline %d arr %s %s ndim %d <",
         (doing_lhs)?"SET":"USE",
         (AR->SYM->dims[d] == MDIM)?'M':'N',
         statement_line_number,
         TOKSTRING(CHILD(node,0)),
         showstorage(getstorage(node)),
         AR->ndim) ;
       for ( i = 0 ; i < AR->ndim ; i++ )
       {
         sprintf(tmpstr,"%c",
           (AR->SYM->dims[i] == MDIM)?'M':((AR->SYM->dims[i] == NDIM)?'N':'?')
                ) ;
         strcat( outstr, tmpstr ) ;
       }
       sprintf(tmpstr,"> <") ; strcat( outstr, tmpstr ) ;
       for ( i = 0 ; i < AR->ndim ; i++ )
       {
         if ( AR->SYM->dims[i] == MDIM || AR->SYM->dims[i] == NDIM )
         {
           sprintf(tmpstr,"%c",
             (AR->SYM->dims[i] == MDIM)?'M':((AR->SYM->dims[i] == NDIM)?'N':'?')
                  ) ;
           strcat( outstr, tmpstr ) ;
         }
       }
       sprintf(tmpstr,"> %s ",
         fullorpart
         ) ;
       strcat( outstr, tmpstr ) ;
     }
     sprintf(tmpstr,"%d ",ofst) ;
     strcat( outstr, tmpstr ) ;
     }
  }
}
            if      (AR->SYM->dims[d] == MDIM)
              {
                LOOP->LI_IS |= MDIM ; 
                AR->Index_IS[d] = LOOP->LI_IS ;
                strcpy(AR->INDEX[d], LOOP->LI);
              }
            else if (AR->SYM->dims[d] == NDIM)
              {
                LOOP->LI_IS |= NDIM ;
                AR->Index_IS[d] = LOOP->LI_IS ;
                strcpy(AR->INDEX[d], LOOP->LI);
              }
          }

          d++;
        }
      }
if (sw_write_db && outstr[0]) fprintf(db_file,"%s\n", outstr) ;
      for ( LOOP = LOOPLIST ; LOOP != NULL ; LOOP=LOOP->next )
      {
        p = (array_t *)my_malloc(sizeof(array_t)) ;
        bcopy(AR,p,sizeof(array_t)) ;
        p->next = LOOP->ARLIST ;
        LOOP->ARLIST=p ;
      }
    }
    break ;
  default :
    break ;
  }
}

is_expr_of(LI,dex)
  char * LI ;
  NodePtr dex ;
{
  int i ;
  if (dex == NULL) return(0) ;
  if ( MAJORKIND(dex) != exK ) return(0) ;
  if ( MINORKIND(dex) == idrefK )
  {
    return ( (strcmp(TOKSTRING(CHILD(dex,0)),LI) == 0) ) ;
  }
  else
  {
    for ( i = 0 ; i < maxChildren ; i++ )
    {
      if (is_expr_of(LI,CHILD(dex,i)))          /* recursion */
      {
        return(1) ;
      }
    }
  }
  return(0) ;
}

/* given a node pointing to an index expression, find all the
   id references therein, and pass back a pointer to the latest
   assigment before this statement */
NodePtr
last_def(node, highest, retsym )
  NodePtr node ;
  int * highest ;
  sym_nodeptr *retsym ;
{
  sym_nodeptr x ;
  NodePtr firstt, retnode ;
  int curr_line ;
  int linex ;

  *highest = 0 ;
  *retsym = NULL ;
  if ( node == NULL ) return(NULL) ;  
  /* find this statement number */
  if ((firstt = (NodePtr)firsttok( node )) != NULL )
    curr_line = atoi(TOKBEGIN(firstt)) ;
  else
    return(NULL) ;
  return(last_def1( node, curr_line, highest, retsym )) ;
}

NodePtr
last_def1( node, curr_line, highest, retsym )
  NodePtr node ;
  int curr_line ;
  int * highest ;
  sym_nodeptr *retsym ; /* return pointer to sym entry that found retnode */
{ 
  int i, hi ;
  sym_nodeptr x ;
  NodePtr firstt, p, retnode ;
  int linex ;
  retnode = NULL ;
  hi = 0 ;
  if ( node == NULL ) return(NULL) ;
#if 0
printf("curr_line %d, highest %d\n",curr_line, *highest ) ;
print_info(stdout,node) ;
#endif
  switch( MINORKIND(node) )
  {
  case idrefK :
    if ( getclass(node) == VARIABLE )
    {
      if ((x=sym_get(TOKSTRING(CHILD(node,0)))) != NULL )
      {
        NodePtr q ;
        for ( q = (NodePtr)x->assigned ; q != NULL ; q = q->user_def )
        {
          firstt = (NodePtr)firsttok( q ) ;
          linex = atoi(TOKBEGIN(firstt)) ;
          if ( linex <= curr_line && linex > *highest )
          {
            *highest = linex ;
            retnode = q ;
            *retsym = x ;
          }
        }
      }
    }
    return(retnode) ;
    break ;
  default :
    for ( i = 0 ; i < maxChildren ; i++ )
    {
      if ((p=last_def1( CHILD(node,i), curr_line, &hi, retsym )) != NULL )
      {
        retnode = p ;
        *highest = hi ;
      }
    }
    return(retnode) ;
    break ;
  }
  return(NULL) ;
}


surgery_add_loop_begin( dim, LOOP, node, range_child, body_child )
  unsigned long dim ;
  loop_t * LOOP ;
  NodePtr node ;
  int range_child ;
  int body_child ;
{
  NodePtr p, q ;
  NodePtr LSNode, LENode, PrevStat, Parent, dummy[DEPENDS_MAXRET] ;
  NodePtr firstt ;
  int linex ;
  char ranges[256] ;
  char spacing[256] ;
  int i ;
  int nret ;
  char * collapsestr ;

/* 970206 if loop range contains an expression of a decomposed
   loop dimension -- for example, mix -- ignore it. */
  for ( p = CHILD(node,range_child) ; p!=NULL ; p = SIBLING(p))
  {
    if (( q = CHILD(p,2) ) != NULL )
    {
    linex = 999999 ;
    if ((firstt = (NodePtr)firsttok(q))  != NULL )
      linex = atoi(TOKBEGIN(firstt)) ;
    for ( i = 0 ; i < cursor_dimtab ; i++ )
    {
      if (depends_on(q, dimtab[i], linex, dummy, 0))
      {
        if ( sw_allloops==0 )
        {
          if ( ! sw_quiet ) 
          {
          fprintf(stderr,
"Advisory: loop at line %d not converted.  Loop range depends on %s\n",
                  linex, dimtab[i]) ;
          }

          dim = 0 ;
        }
#if 0
/* 970529 instead of ignoring it, make the loop range global */
        else if ( sw_treatlocal )
        {

        }
#endif
      }
    }
    }
  }
/* end 970206 */


/* handle loop ending */
  for ( q = NULL, p = CHILD(node,body_child) ; p != NULL ; p = SIBLING(p) )
    { q = p ; }
  if ( q == NULL )
    { fprintf(stderr,"Internal error: no ending statement in loop?\n") ; exit(2) ; }

  linex = -1 ;
  if ((firstt = (NodePtr)firsttok(q))  != NULL )
    linex = atoi(TOKBEGIN(firstt)) ;

/* simplified 970605 -- we're converting all the loops to
   do-enddo anyway, so there's no reason to eliminate the
   continue statements.  Just put the ENDDO at the end */
  if (MINORKIND(q) == enddoK )
  {
    LSNode = q ;
    MINORKIND(q) = 9001 ;   /* morph the node */
  }
  else
  {
    LSNode = newExNode( 9001 ) ;        /* add a node */
    SIBLING(q) = LSNode ;
    ELDER(LSNode) = q ;
  }

  LSNode->user_def = (NodePtr) my_malloc( 1024 ) ;
  spacing[0] = '\0' ;
  for ( i = 0 ; i < LOOP->indentation ; i++ ) strcat(spacing," ") ;
  if ( dim == MDIM || dim == NDIM )
  {
    char dimtag[4] ;
    dimtag[0] = '\0' ;
    if ( sw_tag_enddos ) sprintf(dimtag,"_%c",(dim==MDIM)?'M':'N') ;
    if ((dim == MDIM && sw_collapse_loops_m)||(dim == NDIM && sw_collapse_loops_n))
      collapsestr = "COLLAPSE_" ;
    else
      collapsestr = "" ;
    if ( sw_L && linex != -1 )
      sprintf((char *)LSNode->user_def,"%4d\t%s%s_%sENDDO%s\n",linex,spacing,toolname,collapsestr,dimtag) ;
    else
      sprintf((char *)LSNode->user_def,"%s%s_%sENDDO%s\n",spacing,toolname,collapsestr,dimtag) ;
  }
  else
  {
    sprintf((char *)LSNode->user_def,"%sENDDO\n",spacing) ;
    if ( MINORKIND(node) == doK )
    {
      sprintf(TOKSTRING(CHILD(node,2)),"") ;  /* get rid of label */
    }
  }

  linex = -1 ;
  if ((firstt = (NodePtr)firsttok(node))  != NULL )
    linex = atoi(TOKBEGIN(firstt)) ;

  if ( dim == MDIM || dim == NDIM )
  {
/* handle loop beginning */
  if ((PrevStat = ELDER(node)) != NULL )
  {
    NodePtr x ;

    /* not the first statement */
    LSNode = newExNode( 9000 ) ;
    CHILD(LSNode,0) = CHILD(node, 0) ;
    CHILD(LSNode,1) = CHILD(node, body_child) ;
    x = CHILD(node, body_child) ;
    PARENT(x) = LSNode ;
    SIBLING(LSNode) = SIBLING(node) ;
    SIBLING(PrevStat) = LSNode ;
    ELDER(LSNode) = PrevStat ;
    if ((x = SIBLING(node)) != NULL )
      ELDER(x) = LSNode ;
  }
  else
  {
    int i ;
    NodePtr x ;

    /* first statement; hence, the child of a node */
    if (( Parent = PARENT(node)) == NULL )
      { fprintf(stderr,"Internal error: huh?\n") ; exit(2) ; } 
    for( i = 0 ; i < maxChildren ; i++ )
      { if ( CHILD(Parent,i) == node ) break ; }
    if ( i == maxChildren )
      { fprintf(stderr,"Internal error: unknown child\n") ; exit(2) ; }
    LSNode = newExNode( 9000 ) ;
    CHILD(Parent,i) = LSNode ;
    PARENT(LSNode) = Parent ;
    CHILD(LSNode,0) = CHILD(node, 0) ;
    CHILD(LSNode,1) = CHILD(node, body_child) ;
    x = CHILD(node, body_child) ;
    PARENT(x) = LSNode ;
    SIBLING(LSNode) = SIBLING(node) ;
    ELDER(LSNode) = NULL ;
    if ((x = SIBLING(node)) != NULL )
      ELDER(x) = LSNode ;
  }
  if ( first_e == node ) first_e = LSNode ;

  /* we're gonna cheat and use the user_def field (a NodePtr) in
     this new node to store a string we'll construct that will be
     put as-is into the program for the statement.  Since it will
     actually point to a char*, casting will be used */
  LSNode->user_def = (NodePtr) my_malloc( 1024 ) ;
  getstat( CHILD(node, range_child), 1, ranges ) ;

  spacing[0] = '\0' ;
  for ( i = 0 ; i < LOOP->indentation ; i++ ) strcat(spacing," ") ;

  if ((dim == MDIM && sw_collapse_loops_m)||(dim == NDIM && sw_collapse_loops_n))
    collapsestr = "COLLAPSE_" ;
  else
    collapsestr = "" ;
  if ( sw_L && linex != -1 )
  {
  sprintf((char *)LSNode->user_def,
          "%4d\t%s%s_%sDO_%c( %s, %s )\n",
          linex,
          spacing,
          toolname,
          collapsestr,
          dim==MDIM?'M':'N',
          LOOP->LI, make_upper((char*)strip_nl(strip_lead( ranges ))) ) ;
  }
  else
  {
  sprintf((char *)LSNode->user_def,
          "%s%s_%sDO_%c( %s, %s )\n",
          spacing,
          toolname,
          collapsestr,
          dim==MDIM?'M':'N',
          LOOP->LI, make_upper((char*)strip_nl(strip_lead( ranges ))) ) ;
  }
  }
}


surgery_addset_constants( AR, d, whichdim, body_child )
  array_t * AR ; 
  int d ;
  int whichdim ;
  int body_child ;
{
  NodePtr firstt, ttt ;
  int i ;
  NodePtr Tvar, Idx, Parent, x ;
  NodePtr place, PrevStat, NewNode ;
  char expr[1024] ;
  char dc ;
  char * ds ;
  sym_nodeptr sx, retsym ;
  char varx[32] ;       /* temporary variable name */
  char vx[64] ;
  int new_varx ;
  int linex ;
int thisline, thatline ;

  firstt = (NodePtr) firsttok(AR->Index[AR->SYM->MDEX]) ;

if ( firstt != NULL )
{
/* fprintf(stderr,"... tokbegin %s\n",TOKBEGIN(firstt)) ; */
thisline = atoi(TOKBEGIN(firstt)) ;
}
else
{
/* fprintf(stderr,"... tokbegin ?\n") ; */
thisline = 9999999 ;
}
  switch(whichdim)
  {
  case MDIM :
    Idx = AR->Index[AR->SYM->MDEX] ;
    dc = 'I' ;
    ds = "M" ;
    break ;
  case NDIM :
    Idx = AR->Index[AR->SYM->NDEX] ;
    dc = 'J' ;
    ds = "N" ;
    break ;
  default :
    fprintf(stderr,"bad dim arg %d\n",whichdim);
    exit(10) ;
    break ;
  }

 /* replace the expression with a temporary variable
    and assign the temporary variable with the correct
    value prior to the loop */
  x = SIBLING(Idx) ;
  SIBLING(Idx) = NULL ;
  getstat( Idx, 1, expr ) ;
 /* if the Idx expression is protected leave it alone 3/1/97 */
  if ( MINORKIND(Idx) == idrefK )
  {
   if (!strncmp(TOKSTRING(CHILD(Idx,0)),"noflic",6))
   {
     SIBLING(Idx) = x ;
     if ( !sw_quiet )
     {
     fprintf(stderr,"Advisory: Protected index line %d: %s\n",
             getlinenumber(Idx),expr) ;
     }
     return ;
   }
  }
  if ((place = last_def( Idx, &linex, &retsym )) != NULL )
  {
    int ifl ;
    NodePtr ifn ;
    /* at this point, place contains a pointer to the node that was
       the most recent definition for a term in the index expression.
       retsym contains a pointer to a symbol table entry for the
       identifier in the expression that was defined at place. 
       this symbol table entry also contains iflev, which tells
       the number of conditional level's place was on, and 
       thisif, a pointer to the node for the "if" node at that level
       (the one who's body contains the node for place).  Each of the
       if nodes uses the field user_def to link to it's next level
       up and so on, until NULL.  Thus, by following this chain up
       we can get up to a statement that's at the same level as
       the current global iflev so that the setting of the constant
       won't be subject to a conditional. */
    for ( ifl = retsym->iflev,
          ifn =(NodePtr)(retsym->thisif);
              ifl>(AR->iflev+1);
                ifl--, ifn=ifn->user_def )
    {
       place = place ;
    }
    if ( ifl < 0 ) { fprintf(stderr,"internal error\n") ; exit(23) ; }
    /* at this point, ifn will point to the if statement at the
       same level or lower than us, or it will be null */
       /* test the line number of sibling of IFN */
    if ( ifn != NULL )
    {
      place = ifn ;
      if ( getlinenumber(SIBLING(ifn)) >= getlinenumber( Idx ) )
      {
      /* generate the expression in place */
      /* splice Tvar into list in place of Idx */
        Tvar = copy_tok_node( firsttok(Idx) ) ;
        TOKSTRING(Tvar) = (char*)my_malloc(256) ;
        sprintf(TOKSTRING(Tvar),
        "\n     +%s_G2L_%s(%s,%s)\n     +",
          toolname,ds,strip_nl(strip_lead(expr)),AR->SYM->dimname[d]) ;
        SIBLING(Idx) = x ;

        goto expinplace ;       /* >>>>> GOTO GOTO GOTO !!!!! <<<<< */
      }
    }
/* fprintf(stderr,"place 1 %08x\n", place ) ; */
    if ( place != NULL )
      place = SIBLING(place) ;
/* fprintf(stderr,"place 2 %08x\n", place ) ; */
  }
  SIBLING(Idx) = x ;

  sprintf(vx,"%s,%s|%d",strip_nl(strip_lead(expr)),AR->SYM->dimname[d],linex) ;

  if (( sx = sym_get(vx) ) == NULL )
  {
    sx = sym_add(vx) ;
    sprintf(varx, "%c%sT_%d", dc, toolname, tvarcnt++ ) ;
    strcpy(sx->varx,varx) ;
    new_varx = 1 ;
  }
  else
  {
    strcpy(varx,sx->varx) ;
    new_varx = 0 ;
  }
  {
    char * p ;
    if ((p = (char *)index(vx,'|')) != NULL ) *p = '\0' ;
  }
  if ( new_varx )
  {
    if ( place == NULL )
{
#if 0
    place = first_e ;
    fprintf(stderr,"%s, place was NULL, set to first_e\n",__FILE__ ) ;
#else
/* step place forward through assigns and calls and stop at the first non assign
   or call or at the statement before one we are generating this for in the
   first place. The idea is to put the translations as late as possible in the
   routine but before anything that can change flow of control through this routine. */
    for ( place = first_e ; place != NULL ; place = SIBLING(place) )
    {
      ttt = (NodePtr) firsttok( place ) ;
      if ( ttt != NULL )
      {
        thatline = atoi( TOKBEGIN( ttt ) ) ;
      }
      else
      {
	thatline = 999999999 ;
      }
/* fprintf(stderr,"thatline %d   thisline %d\n", thatline, thisline ) ; */
      if ( thatline >= thisline ) break ;
      if ( SIBLING(place) != NULL )
      {
        if ( MINORKIND(place) != assignK       &&
             MINORKIND(place) != callK         )
        {
          break ;
        }
      }
    }
#endif
}
    if (( PrevStat = ELDER(place)) != NULL )
    {
      NewNode = newExNode( 9001 ) ;
      SIBLING(NewNode) = place ;
      SIBLING(PrevStat) = NewNode ;
      ELDER(NewNode) = PrevStat ;
      if (place != NULL) ELDER(place) = NewNode ;
    }
    else
    {
      if (( Parent = PARENT(place)) == NULL )
        { fprintf(stderr,"Internal error: huh?\n") ; exit(2) ; }
      for( i = 0 ; i < maxChildren ; i++ )
        { if ( CHILD(Parent,i) == place ) break ; }
      if ( i == maxChildren )
        { fprintf(stderr,"error: unknown child 2\n") ; exit(2) ; }
      NewNode = newExNode( 9001 ) ;
      CHILD(Parent,i) = NewNode ;
      PARENT(NewNode) = Parent ;
      SIBLING(NewNode) = place ;
      ELDER(NewNode) = NULL ;
      if ( place != NULL ) ELDER(place) = NewNode ;
    }
    /*  970228 
    first_e = NewNode ;
    */
    NewNode->user_def = (NodePtr) my_malloc( 1024 ) ;
    sprintf((char *)NewNode->user_def,"      %s=%s_G2L_%s(%s)\n",varx,toolname,ds,vx) ;
  
   /* add declaration of temp var to module after last decl.  Note
      that it's possible there is no last decl.  In that case, don't
      do anything -- the variables don't need to be declared because
      the program is (apparently) using implicit typing anyway. */
    place = last_d ;
    if ( place != NULL )
    {
      NewNode = newExNode( 9001 ) ;
      SIBLING(NewNode) = SIBLING(place) ;
      ELDER(NewNode) = place ;
      x = SIBLING(place) ;
      if ( x != NULL )
        ELDER(x) = NewNode ;
      SIBLING(place) = NewNode ;
      last_d = NewNode ;
      NewNode->user_def = (NodePtr) my_malloc( 1024 ) ;
      sprintf((char *)NewNode->user_def, "      INTEGER %s\n", varx) ;
    }
  }

 /* splice Tvar into list in place of Idx */
  Tvar = copy_tok_node( firsttok(Idx) ) ;
  TOKSTRING(Tvar) = (char*)my_malloc(256) ;
  sprintf(TOKSTRING(Tvar), "%s",varx) ;

expinplace:		/* target of >>>>> GOTO GOTO GOTO !!!!! <<<<< */

  SIBLING(Tvar) = SIBLING(Idx) ;
  ELDER(Tvar) = ELDER(Idx) ;
  PARENT(Tvar) = PARENT(Idx) ;

  if ((x = ELDER(Idx)) != NULL)
    { SIBLING(x) = Tvar ; }
  if ((x = PARENT(Idx)) != NULL)
  {
    for (i = 0 ; i < maxChildren ; i++)
      { if ( CHILD(x,i) == Idx ) break ; }
    if ( i < maxChildren )
    { 
      CHILD(x,i) = Tvar ;
    }
  }

  SIBLING(Idx) = NULL ;
  ELDER(Idx) = NULL ;
  PARENT(Idx) = NULL ;
}


static int li_dim ;
static char*  li_LI ;

li_loc2glob2( node )
  NodePtr node ;
{
  NodePtr firstt, rettok[DEPENDS_MAXRET], tok, newnode ;
  char stat[256] ;
  int i, itok ;

  if ( node == NULL ) return ;
  firstt = (NodePtr) firsttok(node) ;
  switch (MINORKIND(node))
  {
  case 9000 :
    li_loc2glob( CHILD(node,1), li_LI, li_dim ) ;
    break ;
  case ifthenelseK :
  case ifthenelseifK :
  case iflogicalK :
    for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ ) rettok[itok] = NULL;
    if ( depends_on( CHILD(node,3), li_LI, atoi(TOKBEGIN(firstt)), rettok, 1))
    {
      for ( itok = 0 ; itok < DEPENDS_MAXRET ; itok++ )
      {
        NodePtr ft ;
        TokInfoPtr q ;

	if ((tok = rettok[itok]) == NULL ) continue ;
        getstat( tok, 1, stat ) ;
        ft=(NodePtr)firsttok( CHILD(node,3) ) ;/* get a token to copy */
        newnode = (NodePtr) copy_tok_node( ft ) ;
        if ((SIBLING(newnode) = SIBLING(tok)) != NULL)
          ELDER(SIBLING(newnode)) = newnode ;
        if ((ELDER(newnode) = ELDER(tok)) != NULL )
          SIBLING(ELDER(newnode)) = newnode ;
        PARENT(newnode) = PARENT(tok) ;
        if ( PARENT(tok) != NULL )
        {
          for (i = 0 ; i < maxChildren ; i++ )
            if ( CHILD(PARENT(tok),i) == tok ) break ;
          CHILD(PARENT(tok),i) = newnode ;
        }
        TOKSTRING(newnode) = (char*)my_malloc(64) ;
        /*
        sprintf(TOKSTRING(newnode),"((%s)-%cdif)", strip_nl(strip_lead(stat)),
                li_dim==MDIM?'i':'j') ;
        */
        sprintf(TOKSTRING(newnode),"(%s_L2G_%c(%s))",
                toolname,
                li_dim==MDIM?'M':'N',
                strip_nl(strip_lead(stat))) ;
      }
    }
    break ;
  }
}

li_loc2glob( node, LI, dim )
  NodePtr node ;
  char * LI ;
  int dim ;
{
  NodePtr tok ;
  int i ;
  if ( node == NULL ) return ;

  li_LI = LI ;
  li_dim = dim ;
  walk_statements1( node, li_loc2glob2, 999, 0) ;

}

NodePtr
copy_tok_node( node )
  NodePtr node ;
{
  NodePtr p, newNode() ;
  TokInfoPtr q ;
  if ( node == NULL ) return(NULL) ;
  if ( MAJORKIND(node) != tokenK ) return(NULL) ;
  q = (struct TokenRecType * ) my_malloc(sizeof(struct TokenRecType)) ;
  *q = *(node->u.Token) ;
  p = (NodePtr) newNode() ;
  *p = *node ;
  p->u.Token = q ;
  return(p) ;
}

cleanup( loop )
  loop_t *loop ;
{
  array_t *ar, *deadar ;
  for ( ar = (loop)->ARLIST ; ar != NULL ; )
  {
    deadar = ar ;
    ar = ar->next ;
    free(deadar) ;
  }
  free(loop) ;
}

getlinenumber( node )
  NodePtr node ;
{
  NodePtr firstt ;

  if ((firstt=(NodePtr)firsttok(node)) != NULL)
  {
    return(atoi(TOKBEGIN(firstt))) ;
  }
  else
  {
    return(-1) ;
  }
}


