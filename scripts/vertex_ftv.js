//######################################################################################################## 
//#                                                                                                    #\\
//#                           LANDTRENDR FTV AND VERTICES                                              #\\
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
//  - this code generates both a ftv stack and a vertex layer for your target area


//##########################################################################################
// START INPUTS
//##########################################################################################


var ltgee = require('users/emaprlab/public:Modules/LandTrendr.js'); 
var gdrive_folder = 'GEE_Outputs';

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


// Define collection parameters
var aoi = ee.FeatureCollection(adksplus); // Draw or upload a polygon
var startYear = 1985;
var endYear = 2022;
var startDay = '07-01';
var endDay = '09-01';
var index = 'NBR';
var maskThese = ['cloud', 'shadow', 'snow', 'water'];
var years = [];                                                           // make an empty array to hold year band names
for (var i = startYear; i <= endYear; ++i) years.push('yr'+i.toString()); // fill the array with years from the startYear to the endYear and convert them to string

// Define the segmentation parameters
// See: https://emapr.github.io/LT-GEE/lt-gee-requirements.html#lt-parameters
var recovery_threshold = 1;
var max_segments = 6;
var run_params = {
   maxSegments: max_segments,
  spikeThreshold: 0.9,
  vertexCountOvershoot: 3,
  preventOneYearRecovery: true,
  recoveryThreshold: recovery_threshold,
  pvalThreshold: 0.05,
  bestModelProportion: 0.75,
  minObservationsNeeded: 6
};

var lt = ltgee.runLT(startYear, endYear, startDay, endDay, aoi, index, [index], run_params, maskThese);
print(lt.getInfo())

// Get the fitted/smoothed index time series (30 year stack, 1 per year)
var nbr_ftv = lt.select('ftv_' + index.toLowerCase() + '_fit')
var nbr_stack = nbr_ftv.arrayFlatten([years]);       
var ftv_desc = 'lt_' + index.toLowerCase() + '_ftv_rt1_' + startYear + '_' + endYear;
Export.image.toDrive({
  image: nbr_stack, 
  description: ftv_desc,
  folder: gdrive_folder,
  crs: aea_wgs84,  
  region: aoi,
  crsTransform: aea_wgs84_transform,
  maxPixels: 1e13
});

// Get a 30 year raster stack (one for each year) where each pixel is 1/0
// indicating whether or not the pixel is a vertex

// From: https://emapr.github.io/LT-GEE/working-with-outputs.html
var lt_band = lt.select('LandTrendr')
var isVertex = ee.Image(
  ee.List.sequence(startYear, endYear)
    .iterate(
      function (year, acc) {
        year = ee.Number(year)
        var years = lt_band.arraySlice(0, 0, 1)
        var isVertex = lt_band
          .arrayMask(years.eq(year))
        isVertex = isVertex
          .updateMask(isVertex.arrayLength(1)) // Mask if no matching year
          .arraySlice(0, 3, 4)
          .arrayGet([0, 0])
          .unmask(0) // Assume no vertex when no data for year
          .byte()
          .rename(year.format('%d'))
        return ee.Image(acc).addBands(isVertex)
      },
      ee.Image([])
    )
) 
var vertex_desc = 'lt_' + index.toLowerCase() + '_is_vertex_rt1' + startYear + '_' + endYear;
Export.image.toDrive({
  image: isVertex, 
  description: vertex_desc,
  folder: gdrive_folder,
  crs: aea_wgs84,  
  region: aoi,
  crsTransform: aea_wgs84_transform,
  maxPixels: 1e13
});