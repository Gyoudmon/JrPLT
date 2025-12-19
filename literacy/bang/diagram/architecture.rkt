#lang typed/racket/base

(provide (all-defined-out))

(require diafun/flowchart)
(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define arch-tree-hwidth 0.75)
(define arch-layer-title-font (desc-font #:weight 'bold #:size 'normal #:family 'monospace))
(define arch-layer-width 700.0)
(define arch-layer-height 64.0)
(define arch-layer-vgap 8.0)
(define arch-layer-hgap 8.0)

(define arch-blank-layer : (->* (Any) ((Listof Any)) Geo)
  (lambda [desc [subnames null]]
    (define label : Geo (geo-text desc arch-layer-title-font))

    (define all-labels : Geo
      (if (pair? subnames)
          (geo-vc-append #:gapsize 4.0
                         label
                         (geo-hc-append* #:gapsize 8.0
                                         (for/list : (Listof Geo) ([sn (in-list subnames)])
                                           (geo-text sn arch-layer-title-font))))
          label))
    
    (geo-cc-superimpose
     (geo-rectangle arch-layer-width arch-layer-height #:fill 'LightGrey)
     all-labels)))

(define arch-layer-name : (-> Any Geo)
  (lambda [name]
    (geo-cc-superimpose (geo-rectangle arch-layer-height arch-layer-height #:fill 'Azure)
                        (geo-text name arch-layer-title-font))))

(define arch-layer : (->* (Any (Listof Any)) (Real) Geo)
  (lambda [desc module-names [width arch-layer-width]]
    (define label : Geo (geo-text desc arch-layer-title-font))
    (define frame : Geo (geo-rectangle width arch-layer-height #:fill 'White))

    (define n (length module-names))
    (define block-width ((default-diaflow-block-width)))

    (if (> n 0)
        (let* ([available (/ (- width (* (add1 n) arch-layer-hgap)) n)]
               [s (if (>= available block-width) 1.0 (/ available block-width))])
          (define sub-labels : Geo
            (geo-hc-append* #:gapsize arch-layer-hgap
                            (for/list : (Listof Geo:Group) ([mod (in-list module-names)])
                              (assert (dia-flow-block mod #:scale s) geo:group?))))

          (geo-cc-superimpose frame
                              (geo-vc-append #:gapsize arch-layer-vgap label sub-labels)))
        (geo-cc-superimpose frame label))))

(define arch-layers : (-> (Listof (Pairof Any (Listof Any))) Geo)
  (lambda [siblings]
    (define n (length siblings))
    (define N (apply + (for/list : (Listof Index) ([self (in-list siblings)]) (length (cdr self)))))
    
    (cond [(> N 1)
           (let* ([unit (/ (- arch-layer-width (* (+ N (sub1 (* n 2))) arch-layer-hgap)) N)])
             (define all-siblings : Geo
               (geo-hc-append* #:gapsize arch-layer-hgap
                               (for/list : (Listof Geo) ([self (in-list siblings)])
                                 (define subN (length (cdr self)))
                                 (arch-layer (car self) (cdr self) (+ (* unit subN) (* (add1 subN) arch-layer-hgap))))))
             
             (geo-vc-append #:gapsize arch-layer-vgap all-siblings))]
          [(= n 1) (arch-layer (caar siblings) (cdar siblings))]
          [else (geo-blank)])))

(define architecture.dia : Geo
  (geo-table* (list (list #false (arch-blank-layer '|应用程序| '(课程项目 演示项目 其他应用)))
                    (list (arch-layer-name '功能层)
                          (arch-layer '|图形用户接口| (list "时间轴" "用户交互" "窗体渲染" "信息提示机制" "可视元素模型")))
                    (list (arch-layer-name '核心层)
                          (arch-layers (list (cons '|物理系统| (list "代数对象" "几何对象" "物理对象" "随机数"))
                                             (cons '|虚拟化| (list "通信系统" "文件系统" "显示器与屏幕" "位置")))))
                    (list (arch-layer-name '基础设施层)
                          (arch-layers (list (cons '|数据类型包装| (list "类型操作函数" "数值函数" "高精度算术" "文件路径"))
                                             (cons '|底层渲染| (list "画笔与笔刷" "字体" "美术资源")))))
                    (list (arch-layer-name '平台无关层)
                          (arch-layers (list (cons '|C++ 运行时| (list "基础数据类型" "STL" "shared_ptr" "filesystem"))
                                             (cons 'SDL2 (list "事件机制" "网络接口" "多媒体")))))
                    (list #false (arch-blank-layer '|操作系统| '(Windows macOS Linux))))
              'cc 'cc arch-layer-hgap arch-layer-vgap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  architecture.dia)
