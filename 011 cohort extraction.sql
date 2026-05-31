--创建firstICU表单提取所有第一次hosp&icu的患者(94458-29092=65366名)
CREATE TABLE oldfirstICU AS SELECT DISTINCT P
.* 
FROM
  mimiciv31.mimiciv_derived.icustay_detail P 
WHERE
  P.hospstay_seq = 1 
  AND icustay_seq = 1;
  
--创建firsticuoverone表单去除ICU住院时间小于1天患者（65366-13369=51997名）
CREATE TABLE oldfirsticuoverone AS SELECT DISTINCT
* 
FROM
  mimiciv31.PUBLIC.oldfirstICU
WHERE
  los_icu >= 1;
  
  
--创建admission表单去除住院前就用过硫酸镁的患者（51997-9406=42591名）
CREATE TABLE oldadmission AS
SELECT DISTINCT f.*
FROM mimiciv31.PUBLIC.oldfirsticuoverone f
WHERE NOT EXISTS (
    SELECT 1
    FROM mimiciv31.mimiciv_hosp.prescriptions p
    WHERE f.subject_id = p.subject_id
      AND p.starttime < f.icu_intime  -- 关键是这里用 <
      AND p.drug ILIKE '%magnesium sulfate%'
      AND p.route ILIKE 'iv'
);
  
  
---admission里icu住院期间使用过硫酸镁的患者（17363/42591名）
SELECT DISTINCT a.*
FROM mimiciv31.PUBLIC.oldadmission a
JOIN mimiciv31.mimiciv_hosp.prescriptions p ON a.subject_id = p.subject_id
WHERE p.starttime > a.icu_intime
  AND p.stoptime < a.icu_outtime
  AND p.drug LIKE '%Magnesium Sulfate%'
  AND p.route LIKE 'IV';
 
--年龄取整
UPDATE mimiciv31.PUBLIC.admission
SET admission_age = ROUND(admission_age);

--提取身高
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN height FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET height = h.height  
FROM mimiciv31.mimiciv_derived.height AS h
WHERE a.stay_id = h.stay_id;  

--提取体重
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN weight FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET weight = fdh.weight  
FROM mimiciv31.mimiciv_derived.first_day_weight AS fdh
WHERE a.stay_id = fdh.stay_id;  

--提取Charlson合并症指数
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN charlson FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS c
SET charlson = fdh.charlson_comorbidity_index
FROM mimiciv31.mimiciv_derived.charlson AS fdh
WHERE c.hadm_id = fdh.hadm_id;

--提取入院首次白细胞、红细胞、血小板、血红蛋白
---白细胞
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN wbc FLOAT;
WITH earliest_cbc AS (
    SELECT 
        cbc.hadm_id, 
        cbc.charttime, 
        cbc.wbc  -- 选择wbc列
    FROM 
        mimiciv31.mimiciv_derived.complete_blood_count血常规 cbc
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.complete_blood_count血常规
        GROUP BY 
            hadm_id
    ) earliest ON cbc.hadm_id = earliest.hadm_id
               AND cbc.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    wbc = cbc.wbc  -- 更新admission表中的wbc列
FROM 
    earliest_cbc cbc
WHERE 
    ad.hadm_id = cbc.hadm_id;

---红细胞
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN rbc FLOAT;
WITH earliest_cbc AS (
    SELECT 
        cbc.hadm_id, 
        cbc.charttime, 
        cbc.rbc  -- 选择rbc列
    FROM 
        mimiciv31.mimiciv_derived.complete_blood_count血常规 cbc
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.complete_blood_count血常规
        GROUP BY 
            hadm_id
    ) earliest ON cbc.hadm_id = earliest.hadm_id
               AND cbc.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    rbc = cbc.rbc  -- 更新admission表中的rbc列
FROM 
    earliest_cbc cbc
WHERE 
    ad.hadm_id = cbc.hadm_id;

---血小板
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN platelet FLOAT;
WITH earliest_cbc AS (
    SELECT 
        cbc.hadm_id, 
        cbc.charttime, 
        cbc.platelet  -- 选择platelet列
    FROM 
        mimiciv31.mimiciv_derived.complete_blood_count血常规 cbc
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.complete_blood_count血常规
        GROUP BY 
            hadm_id
    ) earliest ON cbc.hadm_id = earliest.hadm_id
               AND cbc.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    platelet = cbc.platelet  -- 更新admission表中的platelet列
FROM 
    earliest_cbc cbc
WHERE 
    ad.hadm_id = cbc.hadm_id;

---血红蛋白
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN hemoglobin FLOAT;
WITH earliest_cbc AS (
    SELECT 
        cbc.hadm_id, 
        cbc.charttime, 
        cbc.hemoglobin  -- 选择hemoglobin列
    FROM 
        mimiciv31.mimiciv_derived.complete_blood_count血常规 cbc
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.complete_blood_count血常规
        GROUP BY 
            hadm_id
    ) earliest ON cbc.hadm_id = earliest.hadm_id
               AND cbc.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    hemoglobin = cbc.hemoglobin  -- 更新admission表中的hemoglobin列
FROM 
    earliest_cbc cbc
WHERE 
    ad.hadm_id = cbc.hadm_id;
    
  
--提取生命体征：心率、sbp、dbp
---心率
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN heart_rate FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.heart_rate  -- 选择heart_rate列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    heart_rate = v.heart_rate  -- 更新admission表中的heart_rate列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
  
---sbp
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN sbp FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.sbp  -- 选择sbp列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    sbp = v.sbp  -- 更新admission表中的sbp列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
    
---dbp
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN dbp FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.dbp  -- 选择dbp列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    dbp = v.dbp  -- 更新admission表中的dbp列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
    
--提取MAP
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN map FLOAT;

WITH earliest_c AS (
    SELECT 
        c.stay_id, 
        c.charttime, 
        c.value::FLOAT AS map_value  -- 假设value是存储MAP值的列，并转换为FLOAT类型
    FROM 
        mimiciv31.mimiciv_icu.chartevents c
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime  -- 修正拼写错误       
 FROM 
            mimiciv31.mimiciv_icu.chartevents
        WHERE 
            itemid = '220052'  -- 筛选MAP相关的itemid
        GROUP BY 
            stay_id
    ) earliest 
    ON c.stay_id = earliest.stay_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '220052'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    map = c.map_value  -- 更新map列的值
FROM 
    earliest_c c
WHERE 
    ad.stay_id = c.stay_id;
    
--提取体温、呼吸
---体温
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN temperature FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.temperature  -- 选择temperature列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    temperature = v.temperature  -- 更新admission表中的temperature列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
    
---呼吸
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN resp_rate FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.resp_rate  -- 选择resp_rate列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    resp_rate = v.resp_rate  -- 更新admission表中的resp_rate列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
    
--提取纤维蛋白原、乳酸 (Lactate)、葡萄糖水平、镁水平、钾水平、钠水平、钙水平、氯化物水平、和磷酸盐水平
---纤维蛋白原
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'fibrinogen';
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN fibrinogen FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS fibrinogen_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '51623'  -- 确保这是fibrinogen的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '51623'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET fibrinogen = c.fibrinogen_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

---乳酸LD
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN LD FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS LD_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '50954'  -- 确保这是LD的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '50954'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET LD = c.LD_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;
    
---葡萄糖
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN glucose FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.glucose  -- 选择glucose列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    glucose = v.glucose  -- 更新admission表中的glucose列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;
    
---Mg
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'magnesium';50960
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN magnesium FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS magnesium_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '50960'  -- 确保这是magnesium的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '50960'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET magnesium = c.magnesium_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

---钾
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN potassium FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.potassium  -- 选择potassium列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    potassium = v.potassium  -- 更新admission表中的potassium列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;
    
---钠
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN sodium FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.sodium  -- 选择sodium列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    sodium = v.sodium  -- 更新admission表中的sodium列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;
    
---钙
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN calcium FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.calcium  -- 选择calcium列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    calcium = v.calcium  -- 更新admission表中的calcium列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;
    
---氯化物
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN chloride FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.chloride  -- 选择chloride列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    chloride = v.chloride  -- 更新admission表中的chloride列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;
    
---磷酸盐
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'phosphate';50970
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN phosphate FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS phosphate_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '50970'  -- 确保这是phosphate的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '50970'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET phosphate = c.phosphate_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

---bun
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN bun FLOAT;
WITH earliest_v AS (
    SELECT 
        v.hadm_id, 
        v.charttime, 
        v.bun  -- 选择bun列
    FROM 
        mimiciv31.mimiciv_derived.chemistry生化 v
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.chemistry生化
        GROUP BY 
            hadm_id
    ) earliest ON v.hadm_id = earliest.hadm_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    bun = v.bun  -- 更新admission表中的bun列
FROM 
    earliest_v v
WHERE 
    ad.hadm_id = v.hadm_id;

---肌酐
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN mdrd_est FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    mdrd_est = cb.mdrd_est
FROM 
    mimiciv31.mimiciv_derived.creatinine_baseline肌酐 cb
WHERE 
    ad.hadm_id = cb.hadm_id;
    
---CD4
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'CD4';51131
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN CD4 FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS CD4_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '51131'  -- 确保这是CD4的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '51131'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET CD4 = c.CD4_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

--CD8
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'CD8';51132
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN CD8 FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS CD8_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '51132'  -- 确保这是CD8的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '51132'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET CD8 = c.CD8_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

--CRP
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN crp FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    crp = cb.crp
FROM 
    mimiciv31.mimiciv_derived.inflammation cb
WHERE 
    ad.subject_id = cb.subject_id;
    
--淋巴细胞、中性粒细胞
---淋巴细胞
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'Lymphocyte';51244
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN Lymphocyte FLOAT;
WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS Lymphocyte_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '51244'  -- 确保这是Lymphocyte的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '51244'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET Lymphocyte = c.Lymphocyte_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

---中性粒细胞
select * from mimiciv31.mimiciv_hosp.d_labitems实验室检查字典 where label~* 'Neutrophil';51256
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN Neutrophil FLOAT;

WITH earliest_c AS (
    SELECT 
        c.hadm_id, 
        c.charttime, 
        CASE 
            WHEN c.value ~ '^[0-9]+([.][0-9]+)?$' THEN c.value::FLOAT
            ELSE NULL  -- 或者设置为默认值
        END AS Neutrophil_value
    FROM 
        mimiciv31.mimiciv_hosp.labevents实验室检查 c
    INNER JOIN (
        SELECT 
            hadm_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_hosp.labevents实验室检查
        WHERE 
            itemid = '51256'  -- 确保这是Neutrophil的项目ID
        GROUP BY 
            hadm_id
    ) earliest 
    ON c.hadm_id = earliest.hadm_id
       AND c.charttime = earliest.earliest_charttime
       AND c.itemid = '51256'
)
UPDATE mimiciv31.PUBLIC.admission ad
SET Neutrophil = c.Neutrophil_value
FROM earliest_c c
WHERE ad.hadm_id = c.hadm_id;

--APACHE II评分
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN apache_ii_score FLOAT;
-- Step 1: 确保 admission1 表中有 apache_ii_score 列
ALTER TABLE mimiciv31.PUBLIC.admission ADD COLUMN IF NOT EXISTS apache_ii_score INT;

-- Step 2: 计算 APACHE II 评分
WITH vitals AS (
    SELECT ce.subject_id, ce.hadm_id,
        MIN(ce.charttime) AS first_charttime,
        MAX(CASE WHEN itemid IN (223762, 220045) THEN valuenum END) AS temperature,  -- 体温 (°C)
        MAX(CASE WHEN itemid IN (220052) THEN valuenum END) AS map,  -- 平均动脉压 (MAP)
        MAX(CASE WHEN itemid IN (220050) THEN valuenum END) AS heart_rate,  -- 心率
        MAX(CASE WHEN itemid IN (220210) THEN valuenum END) AS respiratory_rate  -- 呼吸频率
    FROM mimiciv31.mimiciv_icu.chartevents ce
    JOIN mimiciv31.PUBLIC.admission a 
        ON ce.hadm_id = a.hadm_id OR ce.subject_id = a.subject_id
    WHERE ce.itemid IN (223762, 220045, 220052, 220050, 220210)
        AND ce.valuenum IS NOT NULL
    GROUP BY ce.subject_id, ce.hadm_id
),
labs AS (
    SELECT le.subject_id, le.hadm_id,
        MIN(le.charttime) AS first_labtime,
        MAX(CASE WHEN itemid = 50983 THEN valuenum END) AS sodium,  -- 血清钠
        MAX(CASE WHEN itemid = 50971 THEN valuenum END) AS potassium,  -- 血清钾
        MAX(CASE WHEN itemid = 50912 THEN valuenum END) AS creatinine,  -- 肌酐
        MAX(CASE WHEN itemid = 50820 THEN valuenum END) AS pH,  -- 血液 pH
        MAX(CASE WHEN itemid = 51221 THEN valuenum END) AS hct,  -- 血红蛋白 Hct
        MAX(CASE WHEN itemid = 51301 THEN valuenum END) AS wbc  -- 白细胞计数 (WBC)
    FROM mimiciv31.mimiciv_hosp.labevents实验室检查 le
    JOIN mimiciv31.PUBLIC.admission a 
        ON le.hadm_id = a.hadm_id OR le.subject_id = a.subject_id
    WHERE le.itemid IN (50983, 50971, 50912, 50820, 51221, 51301)
        AND le.valuenum IS NOT NULL
    GROUP BY le.subject_id, le.hadm_id
),
gcs AS (
    SELECT ce.subject_id, ce.hadm_id,
        MIN(ce.charttime) AS first_gcstime,
        MIN(valuenum) AS gcs  -- 取最小值 (越小代表患者意识越差)
    FROM mimiciv31.mimiciv_icu.chartevents ce
    WHERE ce.itemid IN (454, 184, 198)  -- 眼睑睁开、言语反应、运动反应
        AND ce.valuenum IS NOT NULL
    GROUP BY ce.subject_id, ce.hadm_id
),
age_info AS (
    SELECT p.subject_id, a.hadm_id,
        EXTRACT(YEAR FROM a.admittime) - EXTRACT(YEAR FROM p.dod) AS age
    FROM mimiciv31.mimiciv_hosp.patients患者信息 p
    JOIN mimiciv31.mimiciv_hosp.admissions入院表 a ON p.subject_id = a.subject_id
)
SELECT a.subject_id, a.hadm_id,
    temp_score + map_score + hr_score + rr_score +
    sodium_score + potassium_score + creatinine_score +
    ph_score + hct_score + wbc_score + gcs_score +
    age_score AS apache_ii_score
INTO TEMP TABLE apache_scores_temp
FROM mimiciv31.PUBLIC.admission a
LEFT JOIN vitals v ON a.hadm_id = v.hadm_id OR a.subject_id = v.subject_id
LEFT JOIN labs l ON a.hadm_id = l.hadm_id OR a.subject_id = l.subject_id
LEFT JOIN gcs g ON a.hadm_id = g.hadm_id OR a.subject_id = g.subject_id
LEFT JOIN age_info ai ON a.hadm_id = ai.hadm_id OR a.subject_id = ai.subject_id
CROSS JOIN LATERAL (
    SELECT 
        CASE WHEN v.temperature < 36 OR v.temperature > 39 THEN 2 ELSE 0 END AS temp_score,
        CASE WHEN v.map < 60 THEN 4 ELSE 0 END AS map_score,
        CASE WHEN v.heart_rate < 40 OR v.heart_rate > 180 THEN 4 ELSE 0 END AS hr_score,
        CASE WHEN v.respiratory_rate < 6 OR v.respiratory_rate > 50 THEN 4 ELSE 0 END AS rr_score,
        CASE WHEN l.sodium < 110 OR l.sodium > 180 THEN 4 ELSE 0 END AS sodium_score,
        CASE WHEN l.potassium < 2.5 OR l.potassium > 7.0 THEN 4 ELSE 0 END AS potassium_score,
        CASE WHEN l.creatinine > 3.5 THEN 4 ELSE 0 END AS creatinine_score,
        CASE WHEN l.pH < 7.15 OR l.pH > 7.7 THEN 4 ELSE 0 END AS ph_score,
        CASE WHEN l.hct < 20 OR l.hct > 60 THEN 4 ELSE 0 END AS hct_score,
        CASE WHEN l.wbc < 1.0 OR l.wbc > 40 THEN 4 ELSE 0 END AS wbc_score,
        CASE WHEN g.gcs < 6 THEN 4 ELSE 0 END AS gcs_score,
        CASE 
            WHEN ai.age >= 75 THEN 6
            WHEN ai.age BETWEEN 65 AND 74 THEN 5
            WHEN ai.age BETWEEN 55 AND 64 THEN 3
            WHEN ai.age BETWEEN 45 AND 54 THEN 2
            ELSE 0
        END AS age_score
) score_calc;

-- Step 3: 更新 admission 表中的 APACHE II 评分
UPDATE mimiciv31.PUBLIC.admission a
SET apache_ii_score = t.apache_ii_score
FROM apache_scores_temp t
WHERE a.subject_id = t.subject_id
AND a.hadm_id = t.hadm_id;

-- Step 4: 删除临时表
DROP TABLE apache_scores_temp;



--SOFA日最高值
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN sofa FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET SOFA = s.max_sofa_24hours
FROM (
    SELECT stay_id, MAX(sofa_24hours) AS max_sofa_24hours
    FROM mimiciv31.mimiciv_derived.sofa
    GROUP BY stay_id
) AS s
WHERE a.stay_id = s.stay_id;



--LODS
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN lods FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    lods = cb.lods
FROM 
    mimiciv31.mimiciv_derived.lods cb
WHERE 
    ad.stay_id = cb.stay_id;
    
--APSIII
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN apsiii FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    apsiii = a.apsiii
FROM 
    mimiciv31.mimiciv_derived.apsiii a
WHERE 
    ad.stay_id = a.stay_id;
    
--sapsii评分
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN sapsii FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    sapsii = a.sapsii
FROM 
    mimiciv31.mimiciv_derived.sapsii a
WHERE 
    ad.stay_id = a.stay_id;
    
--oasis评分
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN oasis FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    oasis = a.oasis
FROM 
    mimiciv31.mimiciv_derived.oasis a
WHERE 
    ad.stay_id = a.stay_id;
    
--血氧饱和度和尿量
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN spo2 FLOAT;
WITH earliest_v AS (
    SELECT 
        v.stay_id, 
        v.charttime, 
        v.spo2  -- 选择spo2列
    FROM 
        mimiciv31.mimiciv_derived.vitalsign生命迹象 v
    INNER JOIN (
        SELECT 
            stay_id, 
            MIN(charttime) AS earliest_charttime
        FROM 
            mimiciv31.mimiciv_derived.vitalsign生命迹象
        GROUP BY 
            stay_id
    ) earliest ON v.stay_id = earliest.stay_id
               AND v.charttime = earliest.earliest_charttime
)
UPDATE mimiciv31.PUBLIC.admission ad
SET
    spo2 = v.spo2  -- 更新admission表中的spo2列
FROM 
    earliest_v v
WHERE 
    ad.stay_id = v.stay_id;
    
--尿量
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN urineoutput FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    urineoutput = a.urineoutput
FROM 
    mimiciv31.mimiciv_derived.urine_output a
WHERE 
    ad.stay_id = a.stay_id;
    
--脑血管疾病、充血性心力衰竭、糖尿病、肾病、肝病和癌症
---脑血管疾病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN cerebrovascular_disease FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    cerebrovascular_disease = a.cerebrovascular_disease
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;
    
---充血性心力衰竭
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN congestive_heart_failure FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    congestive_heart_failure = a.congestive_heart_failure
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;
    
---糖尿病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN diabetes FLOAT;
-- 更新 mimiciv31.PUBLIC.admission 表中的 diabetes 列
UPDATE mimiciv31.PUBLIC.admission AS a
SET diabetes = CASE
    WHEN c.diabetes_without_cc = 1 OR c.diabetes_with_cc = 1 THEN 1
    ELSE 0
END
FROM mimiciv31.mimiciv_derived.charlson AS c
WHERE a.hadm_id = c.hadm_id;

---肾病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN renal_disease FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    renal_disease = a.renal_disease
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;

---癌症
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN malignant_cancer FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    malignant_cancer = a.malignant_cancer
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;

---肝病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN liver_disease FLOAT;
-- 更新 mimiciv31.PUBLIC.admission 表中的 liver_disease 列
UPDATE mimiciv31.PUBLIC.admission AS a
SET liver_disease = CASE
    WHEN c.mild_liver_disease = 1 OR c.severe_liver_disease = 1 THEN 1
    ELSE 0
END
FROM mimiciv31.mimiciv_derived.charlson AS c
WHERE a.hadm_id = c.hadm_id;

---慢性阻塞性肺病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN chronic_pulmonary_disease FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    chronic_pulmonary_disease = a.chronic_pulmonary_disease
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;
    
---周围血管疾病
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN peripheral_vascular_disease FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    peripheral_vascular_disease = a.peripheral_vascular_disease
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;
    
--心肌梗死
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN myocardial_infarct FLOAT;
UPDATE mimiciv31.PUBLIC.admission ad
SET
    myocardial_infarct = a.myocardial_infarct
FROM 
    mimiciv31.mimiciv_derived.charlson a
WHERE 
    ad.hadm_id = a.hadm_id;

--高血压
select * from mimiciv31.mimiciv_hosp.d_icd_diagnoses诊断icd字典 where long_title~* 'Hypertension';
I10 401
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN hypertension FLOAT;
UPDATE mimiciv31.PUBLIC.admission a
SET hypertension = CASE 
    WHEN EXISTS (
        SELECT 1 
        FROM mimiciv31.mimiciv_hosp.diagnoses_icd诊断 d
        WHERE a.subject_id = d.subject_id
        AND (
            d.icd_code similar to  '(I10|401)%' 
        )
    ) THEN 1
    ELSE 0
END;

--机械通气
select * from mimiciv31.mimiciv_hosp.d_icd_diagnoses诊断icd字典 where long_title~* 'ventilation';
I10 401
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN ventilation FLOAT;
UPDATE mimiciv31.PUBLIC.admission a
SET ventilation = CASE 
    WHEN EXISTS (
        SELECT 1 
        FROM mimiciv31.mimiciv_hosp.procedures_icd d
        WHERE a.subject_id = d.subject_id
        AND (
            d.icd_code similar to  '(9390|9670|9671|9672)%' 
        )
    ) THEN 1
    ELSE 0
END;

--使用肾脏替代疗法（是否）
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN rrt FLOAT;
-- 更新 mimiciv31.PUBLIC.admission1 表中的 rrt 列
UPDATE mimiciv31.PUBLIC.admission AS a
SET rrt = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_derived."crrt连续肾脏替代疗法" AS c
        WHERE c.stay_id = a.stay_id
    ) OR EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_derived."rrt肾脏替代疗法" AS r
        WHERE r.stay_id = a.stay_id
    ) THEN 1
    ELSE 0
END;

--抗生素
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN antibiotic FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET antibiotic = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_derived.antibiotic抗生素 AS ab
        WHERE ab.hadm_id = a.hadm_id
    ) THEN 1
    ELSE 0
END;

--vasoactive_agent血管活性药物
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN vasoactive_agent FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET vasoactive_agent = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_derived.vasoactive_agent血管活性药物 AS ab
        WHERE ab.stay_id = a.stay_id
    ) THEN 1
    ELSE 0
END;

--vasopressin血管加压素
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN vasopressin FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET vasopressin = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_derived.vasopressin血管加压素 AS ab
        WHERE ab.stay_id = a.stay_id
    ) THEN 1
    ELSE 0
END;

--补钾
select * from mimiciv31.mimiciv_hosp.prescriptions处方 where drug ~* 'potassium';
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN potassium FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET potassium = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_hosp.prescriptions处方 AS ab
        WHERE ab.hadm_id = a.hadm_id
          and drug ~* 'potassium'
    ) THEN 1
    ELSE 0
END;

--补钙
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN calcium FLOAT;
UPDATE mimiciv31.PUBLIC.admission AS a
SET calcium = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_hosp.prescriptions处方 AS ab
        WHERE ab.hadm_id = a.hadm_id
          and drug ~* 'calcium'
    ) THEN 1
    ELSE 0
END;

--是否使用硫酸镁
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN magnesium_sulfate FLOAT;
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN first_starttime FLOAT;
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN last_stoptime FLOAT;
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN number_of_doses FLOAT;

UPDATE mimiciv31.PUBLIC.admission AS a
SET magnesium_sulfate = CASE
    WHEN EXISTS (
        SELECT 1 FROM mimiciv31.mimiciv_hosp.prescriptions处方 AS ab
        WHERE ab.hadm_id = a.hadm_id
          and drug ~* 'magnesium sulfate'
          and route ~* 'IV'
    ) THEN 1
    ELSE 0
END;

--硫酸镁用药次数、第一次用药时间、最后一次用药结束时间
-- 更新 mimiciv31.PUBLIC.admission 表中的 number_of_doses, first_starttime 和 last_stoptime 列
WITH magnesium_sulfate AS (
    SELECT 
        hadm_id,
        drug,
        route,
        starttime,
        stoptime,
        ROW_NUMBER() OVER (PARTITION BY hadm_id ORDER BY starttime) AS dose_number,
        COUNT(*) OVER (PARTITION BY hadm_id) AS total_doses
    FROM mimiciv31.mimiciv_hosp.prescriptions处方
    WHERE LOWER(drug) ~* 'magnesium sulfate'
      AND LOWER(route) ~* 'IV'
),
first_last_doses AS (
    SELECT 
        hadm_id,
        MIN(starttime) AS first_starttime,
        MAX(stoptime) AS last_stoptime,
        MAX(total_doses) AS number_of_doses
    FROM magnesium_sulfate
    GROUP BY hadm_id
)
UPDATE mimiciv31.PUBLIC.admission AS a
SET number_of_doses = fl.number_of_doses,
    first_starttime = fl.first_starttime,
    last_stoptime = fl.last_stoptime
FROM first_last_doses AS fl
WHERE a.hadm_id = fl.hadm_id;


--硫酸镁用药剂量、用药频次
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN doses FLOAT;
ALTER TABLE mimiciv31.PUBLIC.admission
ADD COLUMN dosing_frequency FLOAT;



---提出用R处理
---补钾
SELECT * 
FROM mimiciv31.mimiciv_hosp.prescriptions处方
WHERE drug ~* 'potassium'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

---补钙calcium
SELECT * 
FROM mimiciv31.mimiciv_hosp.prescriptions处方
WHERE drug ~* 'calcium'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

---硫酸镁用药
SELECT * 
FROM mimiciv31.mimiciv_hosp.prescriptions处方
WHERE drug ~* 'magnesium sulfate'
AND LOWER(route) ~* 'IV'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

---炎症指标pre and post
--CRP（50889）、
SELECT * 
FROM mimiciv31.mimiciv_hosp.d_labitems实验室检查字典
WHERE label ~* 'C-Reactive Protein';

SELECT * 
FROM mimiciv31.mimiciv_hosp.labevents实验室检查
WHERE itemid = '50889'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

--淋巴细胞51244
SELECT * 
FROM mimiciv31.mimiciv_hosp.d_labitems实验室检查字典
WHERE label ~* 'Lymphocyte';

SELECT * 
FROM mimiciv31.mimiciv_hosp.labevents实验室检查
WHERE itemid = '51244'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

--白细胞51300
SELECT * 
FROM mimiciv31.mimiciv_hosp.d_labitems实验室检查字典
WHERE label ~* 'WBC';

SELECT * 
FROM mimiciv31.mimiciv_hosp.labevents实验室检查
WHERE itemid = '51300'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

--中性粒细胞51256
SELECT * 
FROM mimiciv31.mimiciv_hosp.d_labitems实验室检查字典
WHERE label ~* 'Neutrophil';

SELECT * 
FROM mimiciv31.mimiciv_hosp.labevents实验室检查
WHERE itemid = '51256'
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

--乳酸53154 52442 50813
SELECT * 
FROM mimiciv31.mimiciv_hosp.d_labitems实验室检查字典
WHERE label ~* 'Lactate';

SELECT * 
FROM mimiciv31.mimiciv_hosp.labevents实验室检查
WHERE itemid IN (53154, 52442, 50813)
AND subject_id IN (SELECT subject_id FROM mimiciv31.PUBLIC.admission);

--高血压
--机械通气
--使用肾脏替代疗法（是否）
--APACHE II评分

