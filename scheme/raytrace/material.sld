(define-library (raytrace material)
  (export default-material material material?  make-material
          material-color material-set-color!
          material-ambient material-set-ambient!
          material-diffuse material-set-diffuse!
          material-specular material-set-specular!
          material-shininess material-set-shininess!
          material-reflective material-set-reflective!
          material-transparency material-set-transparency!
          material-refractive-index material-set-refractive-index!
          lighting
          refractive-index)
  (import (scheme base)
          (raytrace tuple)
          (raytrace lights))
  (begin
    (define-record-type <material>
      (make-material color ambient diffuse specular shininess
                     reflective transparency refractive-index)
      material?
      (color material-color material-set-color!)
      (ambient material-ambient material-set-ambient!)
      (diffuse material-diffuse material-set-diffuse!)
      (specular material-specular material-set-specular!)
      (shininess material-shininess material-set-shininess!)
      (reflective material-reflective material-set-reflective!)
      (transparency material-transparency material-set-transparency!)
      (refractive-index material-refractive-index material-set-refractive-index!))

    (define (material c a d s l)
      (make-material c a d s l 0 0 1))

    (define (default-material)
      (material (color 1 1 1)
                0.1 0.9 0.9 200))

    (define (lighting material object light point eyev normalv in-shadow?)
      (let* ((matcol (material-color material))
             (col (if (color? matcol) matcol (object 'pattern-at matcol point)))
             (effective-color (color* col (light 'intensity)))
             (lightv (normalize (tuple-sub (light 'position)
                                           point)))
             (ambient (color-scale effective-color
                                   (material-ambient material)))
             (light-dot-normal (dot lightv normalv))
             (diffuse (color 0 0 0))
             (specular (color 0 0 0)))
        (if (and (< 0 light-dot-normal)
                 (not in-shadow?))
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
                        specular))))

    (define (refractive-index m)
      ((cond ((eq? m 'vacuum) 1)
             ((eq? m 'air) 1.00029)
             ((eq? m 'ice) 1.31)
             ((eq? m 'water) 1.333)
             ((eq? m 'olive-oil) 1.47)
             ((eq? m 'glass) 1.52)
             ((eq? m 'sapphire) 1.77)
             ((eq? m 'diamond) 2.417))))))
