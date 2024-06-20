c-----------------------------------------------------------------------
c © A J S Hamilton 2001
c-----------------------------------------------------------------------
      function felp(epoch)
      integer, parameter ::  star10 = selected_real_kind(r=4931,p=18)
      real(kind=star10) :: felp, epoch
c
c        parameters
      include 'frames.par'
c        local (automatic) variables
      real(kind=star10) t
c *
c * Ecliptic latitude of NCP = Dec of ecliptic NP
c * as a function of epoch (e.g. 1950, 2000).
c *
c        RA & Dec epoch in centuries since 1900
      t=(epoch-1900._star10)/100._star10
c        ecliptic latitude of NCP = Dec of ecliptic NP
      felp=90._star10-(E1+t*(E2+t*(E3+t*E4)))
      return
      end
c
