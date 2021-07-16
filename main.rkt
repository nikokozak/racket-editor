#lang racket

(require glfw3)
(require opengl
         opengl/util)
(require ffi/vector)

(glfwInit)

;; Try to force a more recent version of OpenGL than the Compatibility 2.1 version in MacOS
(glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GL_TRUE)
(glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)

(define window (glfwCreateWindow 800 600 "example window" #f #f))

;; Ensure the context is correct
(glfwMakeContextCurrent window)

(define-syntax dangerous
  (syntax-rules ()
    [(_ body ...)
     (begin (define (container) (with-handlers
                  [(exn:fail? (lambda (exn)
                                (display (exn-message exn))
                                (kill window)))]
                                  body ...))
            (container))]))

; (display (glfwGetVersionString)) Debug

;; Our input processing
(define (processInput window-pointer)
  (when (equal? (glfwGetKey window-pointer GLFW_KEY_ESCAPE) GLFW_PRESS)
    (glfwSetWindowShouldClose window-pointer GLFW_TRUE)))

;; Check whether to continue the draw loop
(define (maybe-continue window-pointer)
  (if (= (glfwWindowShouldClose window-pointer) GLFW_TRUE)
      (kill window-pointer)
      (run window-pointer)))

;; Triangle geometry
(define vertices (f64vector -0.5 -0.5 0.0
                            0.5 -0.5 0.0
                            0.0 0.5 0.0))

(define my-buffer 1)
(glGenBuffers my-buffer)
(glBindBuffer GL_ARRAY_BUFFER my-buffer)
(glBufferData GL_ARRAY_BUFFER (f64vector-length vertices) vertices GL_STATIC_DRAW)

;; Our kill function
(define (kill window-pointer)
  (glfwDestroyWindow window-pointer)
  (glfwTerminate))

(dangerous
    (define my-vertex-shader (create-program (load-shader "test.glsl" GL_VERTEX_SHADER)))
    (glUseProgram my-vertex-shader))

; (define my-fragment-shader (create-program (load-shader "fragment.glsl" GL_FRAGMENT_SHADER)))

;; Our draw loop
(define (run window-pointer)
             (with-handlers ([exn:fail? (lambda (exn)
                                  (display (exn-message exn))
                                  (kill window-pointer))])
  (processInput window-pointer)

  (glClearColor 0.2 0.3 0.3 1.0)
  (glClear GL_COLOR_BUFFER_BIT)

  (glfwPollEvents)
  (glfwSwapBuffers window-pointer)

  (maybe-continue window-pointer)))

;; Run the program
(run window)
