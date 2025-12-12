#!/bin/bash

# Path to the folder containing subject folders
subjects_folder="/beegfs_data/scratch/iandrulyte-diffusion"

FA_folder="/beegfs_data/scratch/iandrulyte-diffusion/FA_maps/for_Ieva_FA_maps"

AF_divided_files="/beegfs_data/scratch/iandrulyte-diffusion/Divided_AF_five_parts"

# Create a CSV file for full AF FA stats with header
echo "subj,side,part,min,max,mean" > "$subjects_folder"/af_parts_fa_stats.csv

# Loop through subject folders starting with "t0"
for subj_folder in "$subjects_folder"/t0*
do
  # Check if the current item is a directory
  if [[ -d "$subj_folder" ]]; then
    for side in "L" "R"; do
      subj=$(basename "$subj_folder")

      echo "Generating binary mask for ${subj} ${side}"

      #Generate binary mask of full AF
      scil_compute_streamlines_density_map.py "$subj_folder"/final_outputs/"$subj"/mni_space/bundles/"$subj"__af_"$side"_mni_space.trk --binary 1 "$subj_folder"/final_outputs/"$subj"/mni_space/bundles/"$subj"__af_"$side"_mni_space_binary_mask.nii.gz

      #Compute FA of the whole AF 

      echo "Computing FA for $subj $side"

      fslmaths "$FA_folder"/"$subj"/"$subj"__fa_in_JHU_MNI.nii.gz -mas "$subj_folder"/final_outputs/"$subj"/mni_space/bundles/"$subj"__af_"$side"_mni_space_binary_mask.nii.gz "$FA_folder"/"$subj"/"$subj"__af_"$side"_mni_space_FA_value.nii.gz

      # Get min, max, and mean FA values for full AF
      fa_stats=$(fslstats "$FA_folder"/"$subj"/"$subj"__af_"$side"_mni_space_FA_value.nii.gz -R -M)

      # Extract min, max, and mean values for full AF
      min=$(echo "$fa_stats" | awk '{print $1}')
      max=$(echo "$fa_stats" | awk '{print $2}')
      mean=$(echo "$fa_stats" | awk '{print $3}')

      # Append to full AF FA stats CSV file
      echo "$subj,$side,Full,$min,$max,$mean" >> "$subjects_folder"/af_parts_fa_stats.csv

      # Split AF bundle labelled file
      echo "Splitting AF bundle labelled file for $subj"

      cd "$AF_divided_files"/"$subj"_af_divided_"$side"/
      scil_split_volume_by_ids.py labels_map.nii.gz --out_prefix AF_

      for part in {1..5}
      do
        # Compute FA of different parts of AF 
        echo "Computing FA of different part of AF of $subj"

        fslmaths "$FA_folder"/"$subj"/"$subj"__fa_in_JHU_MNI.nii.gz -mas AF__"$part".nii.gz "$subj"__af_"$side"_mni_space_FA_value_"$part".nii.gz

        # Get min, max, and mean FA values for each part of AF
        fa_stats_part=$(fslstats "$subj"__af_"$side"_mni_space_FA_value_"$part".nii.gz -R -M)

        # Extract min, max, and mean values for each part of AF
        min_part=$(echo "$fa_stats_part" | awk '{print $1}')
        max_part=$(echo "$fa_stats_part" | awk '{print $2}')
        mean_part=$(echo "$fa_stats_part" | awk '{print $3}')

        # Append to full AF FA stats CSV file
        echo "$subj,$side,$part,$min_part,$max_part,$mean_part" >> "$subjects_folder"/af_parts_fa_stats.csv

        echo "This part of FA of AF is done $i"
      done
    done
  fi
done
