import unittest
import numpy as np
from Strain_Tools.strain import strain_tensor_toolbox, configure_functions, velocity_io
from Strain_Tools.strain.models import strain_delaunay_flat, strain_delaunay


class Tests(unittest.TestCase):

    def test_solid_body_rotation(self):
        """
        This example is solid body rotation. Does solid body rotation come out?
        Diagram of vector setup, depicting a solid-body counter-clockwise rotation:
        <.R
         |
         |       ^
         .Q - - .P
        """
        up, vp = 0, 1;
        ur, vr = -1, 0;
        uq, vq = 0, 0;
        xinc, yinc = 1, 1;
        dudx = (up-uq) / xinc;
        dvdx = (vp-vq) / xinc;
        dudy = (ur-uq) / yinc;
        dvdy = (vr-vq) / yinc;
        [exx, exy, eyy, rotation] = strain_tensor_toolbox.compute_strain_components_from_dx(dudx, dvdx, dudy, dvdy);
        print("Testing solid body rotation.")
        print("exx, exy, eyy: %f %f %f" % (exx, exy, eyy));
        print("rotation: %f" % rotation);
        self.assertEqual(exx, 0);
        self.assertEqual(exy, 0);
        self.assertEqual(eyy, 0);
        self.assertEqual(rotation, 1);
        return;

    def test_simple_shear(self):
        """
        This example is simple shear, which has equal parts rotation and shear.
        For diagram, see Turcotte and Schubert Figure 2-23b with point P as origin, points Q and R as mobile
        """
        theta = 0.1  # in radians
        up, vp = 0, 0;
        ur, vr = np.sin(theta), 0;
        uq, vq = 0, 0;
        xinc, yinc = 1, 1;
        dudx = (uq-up) / xinc;
        dvdx = (vq-vp) / xinc;
        dudy = (ur-up) / yinc;
        dvdy = (vr-vp) / yinc;
        [exx, exy, eyy, rotation] = strain_tensor_toolbox.compute_strain_components_from_dx(dudx, dvdx, dudy, dvdy);
        print("Testing simple shear.")
        print("exx, exy, eyy: %f %f %f" % (exx, exy, eyy));
        print("rotation: %f" % rotation);
        strain_quantity = 0.04991671;  # the computed answer
        self.assertEqual(exx, 0);
        self.assertAlmostEqual(exy, strain_quantity);
        self.assertEqual(eyy, 0);
        self.assertAlmostEqual(rotation, -strain_quantity);
        return;

    def test_reading_config(self):
        MyParams = configure_functions.read_strain_config(configfile="example/00_example_strain_config.txt");
        self.assertTrue(MyParams);
        return;

    def test_delaunay_signs(self):
        station1 = velocity_io.StationVel(name="xxxx", elon=-123, nlat=39, e=0, n=0, u=0, se=1, sn=1, su=1);
        station2 = velocity_io.StationVel(name="yyyy", elon=-124, nlat=39, e=-1, n=2, u=0, se=1, sn=1, su=1);
        station3 = velocity_io.StationVel(name="zzzz", elon=-124, nlat=40, e=-3, n=4, u=0, se=1, sn=1, su=1);
        station4 = velocity_io.StationVel(name="wwww", elon=-123, nlat=40, e=0, n=0, u=0, se=1, sn=1, su=1);
        myVelfield = [station1, station2, station3, station4];
        [_, _, _, rot1, exx1, exy1, eyy1] = strain_delaunay.compute_with_delaunay_polygons(myVelfield);
        [_, _, _, rot2, exx2, exy2, eyy2] = strain_delaunay_flat.compute_with_delaunay_polygons(myVelfield);
        print("delaunay-sphere vs delaunay-flat:")
        print('exx:', exx1, ' vs ', exx2)
        print('exy:', exy1, ' vs ', exy2)
        print('eyy:', eyy1, ' vs ', eyy2);
        self.assertLess(abs(exx1[0]-exx1[0]), abs(exx1[0]*0.05));  # less than 5% difference
        self.assertLess(abs(exy1[0]-exy2[0]), abs(exy1[0]*0.05));  # less than 5% difference
        self.assertLess(abs(eyy1[0]-eyy2[0]), abs(eyy1[0]*0.05));  # less than 5% difference
        self.assertLess(abs(rot1[0]-rot2[0]), abs(rot1[0]*0.05));  # less than 5% difference
        return;

    def test_azimuth_math(self):
        # Test angular math functions
        azimuth_array = [0, 1, 179, 0];
        theta, sd = strain_tensor_toolbox.angle_mean_math(azimuth_array);
        self.assertEqual(theta, 0);
        return;

    def test_readvels(self):
        # Test reading velocity files
        datafile = "test/testing_data/NorCal_stationvels.txt"
        myVelfield = velocity_io.read_stationvels(datafile);
        self.assertGreater(len(myVelfield), 5);
        return;


if __name__ == "__main__":
    unittest.main();
