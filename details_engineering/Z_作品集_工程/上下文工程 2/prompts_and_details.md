# source

some source

# prompt

画一幅图（中文），整体风格简明、扼要、科普风；具体内容是一个表格、表达后续内容的类比。表格内可以有一些简明的图标.

【主题】
传统 OS 体系  v.s. LLM 上下文工程体系
【内容】
传统 OS 体系 | LLM 上下文工程体系 | 说明
CPU | LLM 本体（网络结构、权重参数）| 它很牛、但是只有计算能力
内存 | LLM 上下文窗口 | 它是 LLM 的唯一输入，需要包括 system prompt, user prompt, tools, 知识、数据等；它不太大（如 20k tokens）、但它很珍贵（按 tokens 收费）；内容要恰当、别太多、别太少
FileSystem | LLM 上下文窗口的外挂组件 | ContextWindow 放不下的、刷到 FS 中，性价比非常高，如：我已把具体信息写到 /user/data.txt 中，需要时打开文件并捞取具体信息
线程管理与切换 | Multi-Agent 隔离与交互 | 保持 Context Windows 的隔离、单 Context Window 不做过多的事情、注意力不涣散
KV | 关于用户本身的 long-term memory 等 | 长时记忆；跨越 session、跨越时间，仍然能对用户意图捕捉的精准
DB | RAG, Web-Search 等 | 结构化存储大量信息、大量知识，按需检索出结果，加载到 Context Window 中

# extra_info

收集时间：2025.12.13