#lang racket

(require glfw3)
(require opengl)

(glfwInit)

;; Try to force a more recent version of OpenGL than the Compatibility 2.1 version in MacOS
(glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GL_TRUE)
(glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)

(define window (glfwCreateWindow 800 600 "example window" #f #f))

;; Ensure the context is correct
(glfwMakeContextCurrent window)

; (display (glfwGetVersionString))

;; Our run loop
(define (run window-pointer)
  (if (= (glfwWindowShouldClose window-pointer) 1)
      (end window-pointer)
      (run window-pointer)))

;; Our kill function
(define (end window-pointer)
  (glfwDestroyWindow window-pointer)
  (glfwTerminate))

;; Run the program
(run window)
