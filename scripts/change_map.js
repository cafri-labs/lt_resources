//######################################################################################################## 
//#                                                                                                    #\\
//#                           LANDTRENDR DISTURBANCE MAPPING                                  #\\
//#                                                                                                    #\\
//########################################################################################################


// date: 2024-04-11
// author: Madeleine Desrochers | mldesroc@syr.edu

// parameter definitions: https://emapr.github.io/LT-GEE/api.html#getchangemap
// website: https://github.com/eMapR/LT-GEE
// notes: 
//   - you must add the LT-GEE API to your GEE account to run this script. 
//     Visit this URL to add it:https://code.earthengine.google.com/?accept_repo=users/emaprlab/public
//     
//   - use this app to help parameterize: 
//     https://emaprlab.users.earthengine.app/view/lt-gee-change-mapper
// 
//   - Copied from https://code.earthengine.google.com/68e0a1e9fba7ebe5d7dd525ab0ec3e6e which
//     was copied from emapr...
//


//##########################################################################################
// START INPUTS
//##########################################################################################

// CRS info
var aea_wgs84_transform = [30,0,1316955,0,-30,2667285];
var aea_wgs84 = '\
  PROJCS["Projection = Albers Conical Equal Area", \
    GEOGCS["WGS 84", \
      DATUM["WGS_1984", \
        SPHEROID["WGS 84", 6378137.0, 298.257223563, AUTHORITY["EPSG","7030"]], \
        AUTHORITY["EPSG","6326"]], \
      PRIMEM["Greenwich", 0.0], \
      UNIT["degree", 0.0174532925199433], \
      AXIS["Longitude", EAST], \
      AXIS["Latitude", NORTH], \
      AUTHORITY["EPSG","4326"]], \
    PROJECTION["Albers_Conic_Equal_Area"], \
    PARAMETER["central_meridian", -96.0], \
    PARAMETER["latitude_of_origin", 23.0], \
    PARAMETER["standard_parallel_1", 29.5], \
    PARAMETER["false_easting", 0.0], \
    PARAMETER["false_northing", 0.0], \
    PARAMETER["standard_parallel_2", 45.5], \
    UNIT["m", 1.0], \
    AXIS["x", EAST], \
    AXIS["y", NORTH]] \
  '

// define collection parameters
var startYear = 1985;
var endYear = 2023;
var startDay = '07-01';
var endDay = '09-01';
var index = 'NBR';
var maskThese = ['cloud', 'shadow', 'snow', 'water'];

var aoi = ee.FeatureCollection(nys); 
var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js'); 

// define landtrendr parameters
var runParams = { 
  maxSegments:            10,
  spikeThreshold:         0.9,
  vertexCountOvershoot:   3,
  preventOneYearRecovery: false,
  recoveryThreshold:      0.25,
  pvalThreshold:          0.05,
  bestModelProportion:    0.75,
  minObservationsNeeded:  6
};

var target_year = 2022
// define change parameters
var changeParams = {
  delta:  'loss',
  sort:   'greatest',
  year:   {checked:true, start:1985, end:target_year},
  mag:    {checked:true, value:50,  operator:'>'},
  dur:    {checked:true, value:4,    operator:'<'},
  preval: {checked:true, value:300,  operator:'>'},
  mmu:    {checked:true, value:7},
};
changeParams.index = index;

// run landtrendr
var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, [], runParams, maskThese);

// get the change map layers
var changeImg = ltgee.getChangeMap(lt, changeParams);


// export change data to google drive
var exportImg = changeImg.clip(aoi).unmask(0).short();

Export.image.toDrive({
  image: exportImg, 
  description: 'lt_gee_nbr_greatest_defaults_1985-2023_femc_NY', 
  folder: 'GEE_Outputs',
  region: aoi, 
  scale: 30, 
  crs: aea_wgs84,  
  crsTransform: aea_wgs84_transform,  
  maxPixels: 1e13
});


