(define-library (raytrace material)
  (export default-material material material? material-color
          material-ambient material-diffuse
          material-specular material-shininess)
  (import (scheme base)
          (raytrace tuple))
  (begin
    (define-record-type <material>
      (material color ambient diffuse specular shininess)
      material?
      (color material-color)
      (ambient material-ambient)
      (diffuse material-diffuse)
      (specular material-specular)
      (shininess material-shininess))

    (define (default-material)
      (material (color 1 1 1)
                0.1 0.9 0.9 200))))
