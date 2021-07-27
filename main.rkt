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
     (begin
       (define (container)
         (with-handlers [(exn:fail? (lambda (exn)
                                (display (exn-message exn))
                                (kill window)))]
                                  body ...))
            (container))]))

(define-syntax debug
  (syntax-rules ()
    [(_ st body)
     (let [(result body)]
       (display st)
       (display result)
       (newline)
       result)]))

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
  (glfwTerminate)
  (exit 1))

(define vertexShaderSource #("#version 330 core\nlayout(location = 0) in vec3 position;\nvoid main()\n{\n   gl_Position = vec4(position.xyz, 1.0);\n}\0"))
(define fragmentShaderSource #("#version 330 core\nlayout(location = 0) out vec4 color;\nvoid main()\n{\n   color = vec4(1.0, 0.5, 0.2, 1.0);\n}\0"))

(define program
  (create-program
   (load-shader "./vertex.glsl" GL_VERTEX_SHADER)
   (load-shader "./fragment.glsl" GL_FRAGMENT_SHADER)))

#;(define vertexShaderPort (open-input-string vertexShaderSource))

#;(define (makeVertexShader)
  (let* [(vertexShader (glCreateShader GL_VERTEX_SHADER))]
    (glShaderSource vertexShader 1 vertexShaderSource (list->s32vector (for/list [(line (in-vector vertexShaderSource))] (string-length line))))
    (glCompileShader vertexShader) ;; this seems to be working
    (display "Vertex Shader compilation: ")
    (display (glGetShaderiv vertexShader GL_COMPILE_STATUS))
    (newline)
    vertexShader))

#;(define (makeFragmentShader)
  (let* [(fragmentShader (glCreateShader GL_FRAGMENT_SHADER))]
    (glShaderSource fragmentShader 1 fragmentShaderSource (list->s32vector (for/list [(line (in-vector fragmentShaderSource))] (string-length line))))
    (glCompileShader fragmentShader)
    (display "Fragment Shader compilation: ")
    (display (glGetShaderiv fragmentShader GL_COMPILE_STATUS))
    (newline)
    fragmentShader))


#;(dangerous
  (define program (glCreateProgram))
  (glAttachShader program (makeVertexShader))
  (glAttachShader program (makeFragmentShader))
  (glLinkProgram program)
  (display "Program status: ")
  (display (glGetProgramiv program GL_LINK_STATUS))
  #;(display (glGetProgramInfoLog program 512))
  (newline))

#;(dangerous
    (define my-vertex-shader (create-program (debug "Shader result: " (load-shader vertexShaderPort GL_VERTEX_SHADER))))
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
