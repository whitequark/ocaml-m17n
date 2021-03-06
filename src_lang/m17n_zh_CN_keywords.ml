open Parser

let () =
  List.iter (fun (k,v) -> Hashtbl.add M17n_lexer.keywords k v) [
    "和",          AND;
    "为",          AS;
    "断言",        ASSERT;
    "开始",        BEGIN;
    "类",          CLASS;
    "约束",        CONSTRAINT;
    "执行",        DO;
    "完成",        DONE;
    "下至",        DOWNTO;
    "否则",        ELSE;
    "结束",        END;
    "例外",        EXCEPTION;
    "外部",        EXTERNAL;
    "假",          FALSE;
    "取",          FOR;
    "函",          FUN;
    "函数",        FUNCTION;
    "函子",        FUNCTOR;
    "如果",        IF;
    "在",          IN;
    "包括",        INCLUDE;
    "继承",        INHERIT;
    "初始化",      INITIALIZER;
    "推迟",        LAZY;
    "设",          LET;
    "匹配",        MATCH;
    "方法",        METHOD;
    "模块",        MODULE;
    "可变",        MUTABLE;
    "新",          NEW;
    "对象",        OBJECT;
    "的",          OF;
    "打开",        OPEN;
    "或",          OR;
    "私有",        PRIVATE;
    "递归",        REC;
    "签名",        SIG;
    "结构",        STRUCT;
    "那么",        THEN;
    "至",          TO;
    "真",          TRUE;
    "试",          TRY;
    "类型",        TYPE;
    "值",          VAL;
    "虚拟",        VIRTUAL;
    "条件",        WHEN;
    "当",          WHILE;
    "跟",          WITH;

    "求余",           INFIXOP3("mod");
    (*"land",         INFIXOP3("land");*)
    (*"lor",          INFIXOP3("lor");*)
    (*"lxor",         INFIXOP3("lxor");*)
    (*"lsl",          INFIXOP4("lsl");*)
    (*"lsr",          INFIXOP4("lsr");*)
    (*"asr",          INFIXOP4("asr")*)
  ]
