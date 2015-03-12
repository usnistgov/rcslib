/**
 * @file obsDetCfg.hh
 *
 *  @remark ~ NML data structure for Obstacle Detection Configuration.
 *
 *  @author ~ Will Shackleford
 *
 *  @verbatim
 *  ,--------------------------------------------------------------------.
 *  . THIS software was developed at the National Institute of           .
 *  . Standards and Technology by employees of the Federal Government    .
 *  . in the course of their official duties. Pursuant to title 17       .
 *  . Section 105 of the United States Code this software is not         .
 *  . subject to copyright protection and is in the public               .
 *  . domain. NIST's [intelligent mobility] software is an experimental  .
 *  . system. NIST assumes no responsibility whatsoever for its use by   .
 *  . other parties, and makes no guarantees, expressed or implied,      .
 *  . about its quality, reliability, or any other characteristic. We    .
 *  . would appreciate acknowledgement if the software is used. This     .
 *  . software can be redistributed and/or modified freely provided      .
 *  . that any derivative works bear some notice that they are derived   .
 *  . from it, and any modified versions bear some notice that they      .
 *  . have been modified.                                                .
 *   `-------------------------------------------------------------------
 *  @endverbatim
 *
 *  @par Created by Will Shackleford, 2008-09-25 
 **/


#ifndef OBSDETCFG_HH
#define OBSDETCFG_HH

// generate_symbol_lookups=true


/*----------------------------------*
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include "rcs.hh"               // Common RCS definitions
#include "nmlOffsets.hh"        // OBSDETCFG_BASE


/*-----------------*
 * Macro Constants *
 \*---------------*/
#define OBSTACLE_DET_CONFIG_TYPE OBSDETCFG_BASE

/*-----------------*
 * Data Structures *
 \*---------------*/
class OBSTACLE_DET_CONFIG : public NMLmsg
{
public:
  OBSTACLE_DET_CONFIG();
  void update(CMS *);


  bool enforce_min_intensity; 
  float min_intensity; 

  bool enforce_max_intensity; 
  float max_intensity; //default=1.0

  bool enforce_min_range; 
  float min_range;

  bool enforce_max_range; 
  float max_range; //default=10.0

  double roll; 
  double pitch; 
  double yaw; 
  
  bool enforce_floor_threshold; 
  double floor_threshold; 

  bool apply_isolated_obstacle_filter; 
  int isolated_obstacle_filter_window; 
  int isolated_obstacle_filter_min; 

  bool apply_smoothing; 
  int smoothing_size; 
  double max_smoothing_diff; //default=1.0

  bool enable_x_minimum; 
  float x_minimum; //default=-10.0

  bool enable_x_maximum; 
  float x_maximum; //default=+10.0

  bool enable_y_minimum; 
  float y_minimum; //default=-10.0

  bool enable_y_maximum; 
  float y_maximum; //default=+10.0

  bool enable_z_minimum; 
  float z_minimum; //default=-10.0

  bool enable_z_maximum; 
  float z_maximum; //default=+10.0

  int stop_at_obs_pixel;
};



/*--------------------*
 * Function Prototype *
 \*------------------*/
int obsDetCfg_format (NMLTYPE type, void *buf, CMS *cms);


#endif // OBSDETCFG_HH
