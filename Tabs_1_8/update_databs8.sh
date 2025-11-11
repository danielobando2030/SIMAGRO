## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs8
sudo mkdir /srv/shiny-server/Abs8


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs8

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs8/ /srv/shiny-server/Abs8 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs8

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
