#pragma once

#include "Vec.h"
#include <cmath>
#include <cstddef>

constexpr Vec<2> e_x = {1.0, 0.0};
constexpr Vec<2> e_y = {0.0, 1.0};

struct PodConfig {
    Vec<2> pos;
    double heading;
    double wheel_radius;
};

struct PodFactors {
    double inv_x_fac;
    double inv_y_fac;
    double inv_rot_fac;
};
double deg2rad(double deg) {
    return deg / 180.0 * M_PI;
}

PodFactors cal_factors(const PodConfig &pc) {
    Vec<2> d = {cos(deg2rad(pc.heading)), sin(deg2rad(pc.heading))};
    double inv_x_fac = pc.wheel_radius * (d.dot(e_x));
    double inv_y_fac = pc.wheel_radius * (d.dot(e_y));
    double inv_rot_fac = 0;

    return {inv_x_fac, inv_y_fac, inv_rot_fac};
}

struct Twist {
    Vec<2> linear_vel;
    double rot_vel;
};

template<size_t num_pods>
class Odometry {
public:
    Odometry();
    Twist calculate_twist() {}
    const std::array<PodConfig, num_pods> pod_configs;
};
