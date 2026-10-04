import streamlit as st
import subprocess
import json
import uuid
import os
import pandas as pd

st.set_page_config(page_title="鸭芯智选 · Streamlit网页版", layout="wide")
st.title("🦆 鸭芯智选（简单模式）Python调用R后端")

# 临时目录，存放上传文件，云环境/tmp可读写
TMP_DIR = "/tmp"
if not os.path.exists(TMP_DIR):
    os.makedirs(TMP_DIR)

# ---------------------- 1. 页面表单 UI ----------------------
st.subheader("① 上传数据文件")
feed_file = st.file_uploader("采食表（Excel xlsx）", type=["xlsx","xls"])
bw_file = st.file_uploader("体重表（Excel xlsx）", type=["xlsx","xls"])
ped_file = st.file_uploader("系谱文件（可选 xlsx）", type=["xlsx","xls"])
idmap_file = st.file_uploader("ID对照表（可选 xlsx）", type=["xlsx","xls"])

st.subheader("② 设置育种目标（可多选）")
goal_save = st.checkbox("吃得省（料重比低）", value=True)
goal_grow = st.checkbox("长得快（日增重高）", value=True)
goal_fat = st.checkbox("皮脂厚", value=False)
goal_meat = st.checkbox("胸肌厚", value=False)
goal_rhythm = st.checkbox("采食规律", value=False)

st.subheader("③ 留种参数")
ratio = st.slider("留种比例 %", min_value=5, max_value=80, value=30)
sex_balance = st.checkbox("尽量公母均衡留种", value=True)

run_btn = st.button("🚀 开始运行育种计算", type="primary")

# ---------------------- 2. 点击运行后逻辑 ----------------------
if run_btn:
    if not feed_file or not bw_file:
        st.error("请必须上传【采食表】和【体重表】！")
        st.stop()

    # 会话唯一ID，解决多用户并发冲突
    session_uid = str(uuid.uuid4())

    # 保存上传文件到服务器临时路径
    feed_tmp = os.path.join(TMP_DIR, f"feed_{session_uid}.xlsx")
    bw_tmp = os.path.join(TMP_DIR, f"bw_{session_uid}.xlsx")
    with open(feed_tmp, "wb") as f:
        f.write(feed_file.read())
    with open(bw_tmp, "wb") as f:
        f.write(bw_file.read())

    ped_tmp = None
    if ped_file:
        ped_tmp = os.path.join(TMP_DIR, f"ped_{session_uid}.xlsx")
        with open(ped_tmp, "wb") as f:
            f.write(ped_file.read())

    idmap_tmp = None
    if idmap_file:
        idmap_tmp = os.path.join(TMP_DIR, f"idmap_{session_uid}.xlsx")
        with open(idmap_tmp, "wb") as f:
            f.write(idmap_file.read())

    # 组装育种目标列表
    goals = []
    if goal_save: goals.append("save")
    if goal_grow: goals.append("grow")
    if goal_fat: goals.append("fat")
    if goal_meat: goals.append("meat")
    if goal_rhythm: goals.append("rhythm")
    if len(goals) == 0:
        st.warning("至少选择1个育种目标，自动选择【吃得省】")
        goals = ["save"]

    # 构造R读取的json配置
    cfg_json_path = os.path.join(TMP_DIR, f"cfg_{session_uid}.json")
    out_json_path = os.path.join(TMP_DIR, f"out_{session_uid}.json")

    config = {
        "feed_files": [feed_tmp],
        "bw_files": [bw_tmp],
        "ped_path": ped_tmp,
        "idmap_path": idmap_tmp,
        "goals": goals,
        "ratio": ratio / 100.0,
        "sex_balance": sex_balance,
        "output": out_json_path
    }
    with open(cfg_json_path, "w", encoding="utf-8") as f:
        json.dump(config, f, ensure_ascii=False)

    st.info("🔄 正在调用R脚本计算，请等待……")
    # 执行R脚本，注意：工作目录必须是R脚本所在根目录！！
    work_dir = os.path.dirname(os.path.abspath(__file__))
    cmd = [
        "Rscript",
        "duck_tool_simple_shiny.R",
        cfg_json_path,
        out_json_path
    ]
    try:
        proc = subprocess.run(
            cmd,
            cwd=work_dir,
            capture_output=True,
            text=True,
            timeout=300
        )
    except subprocess.TimeoutExpired:
        st.error("❌ 运行超时！数据量太大，请减少数据量")
        st.stop()

    # 打印R输出日志，调试用
    st.expander("📋 R脚本运行日志").code(proc.stderr + "\n" + proc.stdout)

    if proc.returncode != 0:
        st.error(f"❌ R程序出错，返回码：{proc.returncode}")
        st.stop()

    if not os.path.exists(out_json_path):
        st.error("❌ R没有输出结果文件！")
        st.stop()

    # 读取R输出json结果
    with open(out_json_path, "r", encoding="utf-8") as f:
        res_data = json.load(f)

    # ----------------展示结果----------------
    st.success("✅ 计算完成！")
    st.subheader("📊 运行汇总信息")
    df_summary = pd.DataFrame(res_data["summary"])
    st.dataframe(df_summary, use_container_width=True)

    st.subheader("🏆 留种名单")
    df_retain = pd.DataFrame(res_data["retained"])
    st.dataframe(df_retain, use_container_width=True)

    st.subheader("⚠️ 异常个体预警")
    df_anomaly = pd.DataFrame(res_data["anomalies"])
    st.dataframe(df_anomaly, use_container_width=True)

    # 下载留种名单csv
    csv_data = df_retain.to_csv(index=False).encode("utf‑8‑sig")
    st.download_button(
        label="📥 下载留种名单CSV",
        data=csv_data,
        file_name=f"留种名单_{session_uid}.csv",
        mime="text/csv"
    )

    # ----------------清理临时文件----------------
    for f_del in [feed_tmp, bw_tmp, ped_tmp, idmap_tmp, cfg_json_path, out_json_path]:
        if f_del and os.path.exists(f_del):
            os.unlink(f_del)