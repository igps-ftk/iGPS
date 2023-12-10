
Brief documentation for tenv3 format.

Example line:

-------------------
COVE 10JUL28 2010.5708 55405 1594 4 -112.8  -3815 -0.41548   4276712 0.86481  1687  0.35637  0.1800 0.00065 0.00067 0.00328  0.05829 -0.56488  0.04405
-------------------

Meaning of columns:

1.  COVE      station name
2.  10JUL28   date
3.  2010.5708 decimal year
4.  55405     modified Julian day
5.  1594      GPS week
6.  4         day of GPS week
7.  -112.8    longitude (degrees) of reference meridian
8.  -3815     eastings (m), integer portion (from ref. meridian)
9.  -0.41548  eastings (m), fractional portion
10.  4276712  northings (m), integer portion (from equator)
11.  0.86481  northings (m), fractional portion
12.  1687     vertical (m), integer portion
13.  0.35637  vertical (m), integer portion
14. 0.1800    antenna height (m) assumed from Rinex header
15. 0.00065   east sigma (m)
16. 0.00067   north sigma (m)
17. 0.00328   vertical sigma (m)
18.  0.05829  east-north correlation coefficient
19. -0.56488  east-vertical correlation coefficient
20.  0.04405  north-vertical correlation coefficient

NOTES:

(1) Note the initial space in front of some of the positive numbers.
This is not the case if the number can never be negative (sigmas).

(2) Reference longitudes are every 0.1 degrees, which is ~11 km wide at the equator, and ~500 m wide at HOWE, the closest bedrock station to the South Pole.  There are 3601 zones from -180.0 to +180.0.  Technically the two extremal zones -180.0 and +180.0 are identical, but the sign is actually determined by the longitude of the site (so they have half the width).

(3) Northings are are accurate to 1 micron using a very fast approximate formula (versus numerical integration). Easting accuracy actually depends on Northing velocity, but is < 0.1 mm for tectonic velocities. (This is a general problem for UTM systems).

(4) The integer portion of any coordinate stays the same if crossing an integer boundary, so that the fractional portion will go beyond 1 or less than -1.  This makes it easy to use the fractional portion for plotting.

(5) But if the time series drifts or jumps more than 10 meters, then the integer portion is re-initialized.  This keeps the fractional portion within bounds of the format, while allowing for the detection of duplicate sites (which can be on different continents, and sometimes can be locally misnamed or renamed).

(6) Continuous time series has been verified to be preserved in the most extreme case of AMU2 ~50 meters from the South Pole, which moves 10 meters per year.  In this extreme case, 0.1 degree zones of longitude are only 10 cm apart(!).  If AMU2 passes this test, then probably everything does unless there is a duplicate station, etc.

(7) Very large sigmas are set to 9.99999

(8) The bottom line is -- simply plot the fractional parts!  The integer parts can be used to detect problems.
