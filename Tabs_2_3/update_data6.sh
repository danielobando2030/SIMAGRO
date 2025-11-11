## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Ind1
sudo mkdir /srv/shiny-server/Ind1


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind1

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Ind1/ /srv/shiny-server/Ind1 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind1

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
