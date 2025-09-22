session <- ers_session()

# search for datasets
datasets_df <- ers_dataset_search(
  session,
  dataset_name = "landsat",
  spatial_filter = filter_spatial(-115, 50, -114, 51),
  temporal_filter = filter_temporal("2020-01-01", "2020-12-31")
)

# search for scenes of a specific dataset
dataset_alias <- "landsat_ot_c2_l2"

scenes_df <- ers_scene_search(
  session,
  dataset_name = dataset_alias,
  spatial_filter = filter_spatial(-115, 50, -114, 51),
  temporal_filter = filter_temporal("2020-01-01", "2020-12-31"),
  cloud_filter = filter_cloud(0, 10)
)

# create a scene list (analogous to the "cart" in the web interface)
scene_list_id <- as.character(lubridate::now())
ers_scene_list_add(session, "landsat_ot_c2_l2", scenes_df, scene_list_id)

ers_scene_list_summary(session, scene_list_id)

# get the results of the scene list
scene_list <- ers_scene_list_get(session, scene_list_id)

# identify available products for each scene in the list
products <- ers_scene_products(session, "landsat_ot_c2_l2", scene_list_id, scene_list)

# prepare a download request
order_label <- "batch2020"
download_queue <- ers_download_request(session, products, label = order_label)

# download the available products
dst <- "/Volumes/Samsung SSD/Landsat"
dir.create(dst, recursive = TRUE)

ers_download_retrieve(
  session,
  download_queue,
  label = order_label,
  out_dir = dst
)
