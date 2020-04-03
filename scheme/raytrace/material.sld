(define-library (raytrace material)
  (export default-material material material? material-color
          material-ambient material-diffuse
          material-specular material-shininess
          lighting)
  (import (scheme base)
          (raytrace tuple)
          (raytrace lights))
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
                0.1 0.9 0.9 200))

    (define (lighting material light point eyev normalv)
      (let* ((effective-color (color* (material-color material)
                                      (light 'intensity)))
             (lightv (normalize (tuple-sub (light 'position)
                                           point)))
             (ambient (color-scale effective-color
                                   (material-ambient material)))
             (light-dot-normal (dot lightv normalv))
             (diffuse (color 0 0 0))
             (specular (color 0 0 0)))
        (if (< 0 light-dot-normal)
            (begin
              (set! diffuse
                (color-scale effective-color
                             (* (material-diffuse material)
                                light-dot-normal)))
              (let* ((reflectv (reflect (tuple-neg lightv)
                                        normalv))
                     (reflect-dot-eye (dot reflectv eyev)))
                (if (< 0 reflect-dot-eye)
                    (set! specular
                      (color-scale (light 'intensity)
                                   (* (material-specular material)
                                      (expt reflect-dot-eye
                                            (material-shininess material)))))))))

        (color+ ambient
                (color+ diffuse
                        specular))))))
