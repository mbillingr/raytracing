# The Ray Tracer Challenge

This repository contains my two implementations of [The Ray Tracer Challenge](https://pragprog.com/book/jbtracer/the-ray-tracer-challenge). One of the implementations was written in Scheme (portable R7RS) to test the Scheme interpreter I am working on. The other implementation was written in Rust, whose superior performance allowed me to implement a few additional features.

# Features of the Rust implementation

- The basic ray tracer from the book
- write to PNG format 
- Height fields
- Adaptive multisampling
- Area lights and soft shadows
- Depth of field
- Photon mapping

## Adaptive multisampling

Although adaptive multisampling is a very simple technique, it greatly facilitates eye candy effects like anti-aliasing, soft shadows, and depth of field. Thus, it deserves a bit of an explatation...

The idea is simple:
1. Randomly shoot a few rays through the area covered by a pixel.
2. Compute the variance of the rays' resulting colors.
3. Keep shooting more rays until the standard error of the average color falls below a chosen threshold.

Only few rays are cast for low variance pixels and as many rays as required are cast for high variance pixels. High variance pixels include, for example, edge pixels. Some rays through an edge pixel will hit an object, some rays will miss it. The more rays are hit by the object the more it contributes to the final pixel color. Thus, adaptive multisampling naturally gives rise to an anti-aliasing effect on edges. 

Similarly, the implementation of soft shadows cast by area lights becomes beautifully simple. From the point being shaded, we simply cast a single light ray to a random point on the light's surface. If the point is partially shadowed its color has high variance and adpative multisampling takes care of casting the required number of rays.

# Gallery
The following images were rendered with the Rust implementation of the ray tracer.

### Soft Shadows
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/soft_shadow.png)

### Depth of Field
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/depth-of-field.png)

### Photon Mapping
##### Ray traced direct light, photon mapped diffuse indirect light and caustics
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/photon-map-03_traced_direct_diffuse_and_caustic_photons.png)

##### All light photon mapped
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/photon-map-02-direct_and_caustic_photons.png)

##### Ray traced direct light only
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/photon-map-01-trace_direct_only.png)


### Chapter 16: CSG objects (dice and lens)
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-16.png)

### Chapter 15: Triangle mesh
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-15.png)

### Chapter 14: Groups
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-14.png)

### Chapter 13: Cylinders
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-13.png)

### Chapter 12: Cubes
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-12.png)

### Chapter 11: Reflection and Refraction
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-11a.png)
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-11b.png)

### Chapter 10: Patterns
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-10.png)

### Chapter 9: Planes
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-09.png)

### Chapter 8: Shadows
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-08.png)

### Chapter 7: Scenes 
Rendered with shadows from chapter 8.
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-07.png)

### Chapter 6: Lighting 
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-06.png)

### Chapter 5: Ray-Sphere intersections 
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-05.png)

### Chapter 4: Transformations

### Chapter 3: Matrices

### Chapter 2: Drawing on a Canvas
![](https://raw.githubusercontent.com/mbillingr/raytracing/master/rust/pictures/chapter-02.png)

### Chapter 1: Tuples, Points, and Vectors
