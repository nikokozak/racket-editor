#lang racket

(require glfw3)

(glfwInit)
(define window (glfwCreateWindow 800 600 "example window" #f #f))
(sleep 3)
(glfwDestroyWindow window)
(glfwTerminate)
