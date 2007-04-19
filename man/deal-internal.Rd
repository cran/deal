%                               -*- Mode: Rd -*- 
% deal-internal.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Wed Jan 07 10:32:46 2004
% Last Modified By: Claus Dethlefsen
% Last Modified On: Fri Apr 20 09:44:24 2007
% Update Count    : 39
% Status          : Unknown, Use with caution!
% 
\name{deal-internal}

\alias{printline}
\alias{post}
\alias{reinis}
\alias{post0}
\alias{postc}
\alias{postc0c}
\alias{postcc}
\alias{learnnode}
\alias{udisclik}
\alias{addrandomarrow}
\alias{turnrandomarrow}
\alias{deleterandomarrow}
\alias{addarrows}
\alias{addarrow}
\alias{turnarrow}
\alias{removearrow}
\alias{cycletest}
\alias{findleaf}
\alias{conditional.cont}
\alias{conditional.disc}
\alias{conditional}
\alias{cond.node}
\alias{cond}
\alias{elementin}
\alias{findex}
\alias{jointdisc}
\alias{jointcont}
\alias{localmaster}
\alias{postdist}
\alias{postdist.node}
\alias{nwequal}
\alias{inspectprob}


\alias{DealTestClass-class}
\alias{networkclass-class}
\alias{integerOrNULL-class}

\alias{graphComponents-methods}
\alias{graphComponents,networkclass-method}
\alias{setGraphComponents-methods}
\alias{setGraphComponents,networkclass-method}
\alias{Str-methods}
\alias{Str,networkclass-method}
\alias{label,DealTestClass-method}
\alias{width,DealTestClass-method}
\alias{dynamic.Graph-methods}
\alias{dynamic.Graph,networkclass-method}
\alias{testEdge-methods}
\alias{modifyModel-methods}
\alias{modifyModel,networkclass-method}
\alias{testEdge,networkclass-method}

\title{deal internal functions}

\description{
  These are functions internally called by other functions
  in the package \code{deal} and not meant to be called by the user.
}

\usage{
printline(s="-",n=60)

post   (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postc  (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postcc (mu,tau,rho,phi,y,z,timetrace=FALSE) 
post0  (mu,tau,rho,phi,y,timetrace=FALSE) 
postc0c(mu,tau,rho,phi,y,timetrace=FALSE) 

learnnode(node,nw,df,prior=jointprior(nw),timetrace=FALSE)
udisclik(node,nw,df)

addrandomarrow(nw,data,prior,trylist=vector("list",size(nw)),nocalc=FALSE,
               timetrace=FALSE)
turnrandomarrow(nw,data,prior,trylist=vector("list",size(nw)),nocalc=FALSE,
               timetrace=FALSE)
deleterandomarrow(nw,data,prior,trylist=vector("list",size(nw)),nocalc=FALSE,
               timetrace=timetrace)

addarrows(nw, node, data, prior,trylist=vector("list",size(nw)))

addarrow   (nw,df,prior,trylist=vector("list",size(nw))) 
turnarrow  (nw,df,prior,trylist=vector("list",size(nw))) 
removearrow(nw,df,prior,trylist=vector("list",size(nw))) 

cycletest(nw)
findleaf (nw) 

conditional.cont(A,mu,nu,rho,phi) 
conditional(A,master,nw) 
conditional.disc(A,master) 
cond.node(node,nw,nw.prior=jointprior(nw)) 

elementin(nw,nwl)

findex (i, dim, config=TRUE)

jointdisc(nw,timetrace=FALSE)
jointcont(nw,timetrace=FALSE)

localmaster(family,nw,prior=jointprior(nw))

postdist(nw) 
postdist.node(nd,nw,vtype = "mode")

nwequal(nw1,nw2)

inspectprob(nw,unitscale=20,cexscale=8,
            arrowlength=.25,xr=c(0,350),yr=xr,...) 

	     }

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{cld@rn.dk}.
}


\keyword{internal}
