// Copyright (c) ppy Pty Ltd <contact@ppy.sh>. Licensed under the MIT Licence.
// See the LICENCE file in the repository root for full licence text.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using osu.Framework;
using osu.Framework.Allocation;
using osu.Framework.Bindables;
using osu.Framework.Graphics;
using osu.Framework.Graphics.Containers;
using osu.Framework.Logging;
using osu.Game.Beatmaps.Legacy;
using osu.Game.Graphics.Containers;
using osu.Game.Graphics.UserInterfaceV2;
using osu.Game.Online.API;
using osu.Game.Online.API.Requests.Responses;
using osu.Game.Overlays;
using osu.Game.Rulesets;
using osu.Game.Rulesets.Difficulty;
using osu.Game.Rulesets.Mods;
using osu.Game.Rulesets.Scoring;
using OsuParsers.Database.Objects;
using OsuParsers.Decoders;
using OsuParsers.Enums.Database;
using osuTK.Graphics;
using PerformanceCalculatorGUI.Components;
using PerformanceCalculatorGUI.Components.TextBoxes;
using PerformanceCalculatorGUI.Configuration;
using ParsedScore = OsuParsers.Database.Objects.Score;


namespace PerformanceCalculatorGUI.Screens
{
    public partial class ScoredbScreen : PerformanceCalculatorScreen
    {
        [Cached]
        private OverlayColourProvider colourProvider = new OverlayColourProvider(OverlayColourScheme.Plum);

        private StatefulButton calculationButton;
        private VerboseLoadingLayer loadingLayer;

        private GridContainer layout;

        private FillFlowContainer scores;

        private LabelledTextBox usernameTextBox;
        private Container userPanelContainer;
        private UserCard userPanel;

        private string currentUser;

        private CancellationTokenSource calculationCancellatonToken;

        [Resolved]
        private NotificationDisplay notificationDisplay { get; set; }

        [Resolved]
        private APIManager apiManager { get; set; }

        [Resolved]
        private Bindable<RulesetInfo> ruleset { get; set; }

        [Resolved]
        private SettingsManager configManager { get; set; }

        [Resolved]
        private RulesetStore rulesets { get; set; }

        public override bool ShouldShowConfirmationDialogOnSwitch => false;

        private const float username_container_height = 40;

        public ScoredbScreen()
        {
            RelativeSizeAxes = Axes.Both;
        }

        [BackgroundDependencyLoader]
        private void load()
        {
            InternalChildren = new Drawable[]
            {
                layout = new GridContainer
                {
                    RelativeSizeAxes = Axes.Both,
                    ColumnDimensions = new[] { new Dimension() },
                    RowDimensions = new[] { new Dimension(GridSizeMode.Absolute, username_container_height), new Dimension(GridSizeMode.Absolute), new Dimension() },
                    Content = new[]
                    {
                        new Drawable[]
                        {
                            new GridContainer
                            {
                                Name = "Settings",
                                Height = username_container_height,
                                RelativeSizeAxes = Axes.X,
                                ColumnDimensions = new[]
                                {
                                    new Dimension(),
                                    new Dimension(GridSizeMode.AutoSize)
                                },
                                RowDimensions = new[]
                                {
                                    new Dimension(GridSizeMode.AutoSize)
                                },
                                Content = new[]
                                {
                                    new Drawable[]
                                    {
                                        usernameTextBox = new ExtendedLabelledTextBox
                                        {
                                            RelativeSizeAxes = Axes.X,
                                            Anchor = Anchor.TopLeft,
                                            Label = "Username",
                                            PlaceholderText = "peppy",
                                            CommitOnFocusLoss = false
                                        },
                                        calculationButton = new StatefulButton("Start calculation")
                                        {
                                            Width = 150,
                                            Height = username_container_height,
                                            Action = () => { calculateProfile(usernameTextBox.Current.Value); }
                                        }
                                    }
                                }
                            },
                        },
                        new Drawable[]
                        {
                            userPanelContainer = new Container
                            {
                                RelativeSizeAxes = Axes.X,
                                AutoSizeAxes = Axes.Y
                            }
                        },
                        new Drawable[]
                        {
                            new OsuScrollContainer(Direction.Vertical)
                            {
                                RelativeSizeAxes = Axes.Both,
                                Child = scores = new FillFlowContainer
                                {
                                    RelativeSizeAxes = Axes.X,
                                    AutoSizeAxes = Axes.Y,
                                    Direction = FillDirection.Vertical
                                }
                            }
                        },
                    }
                },
                loadingLayer = new VerboseLoadingLayer(true)
                {
                    RelativeSizeAxes = Axes.Both
                }
            };

            usernameTextBox.OnCommit += (_, _) => { calculateProfile(usernameTextBox.Current.Value); };

            if (RuntimeInfo.IsDesktop)
                HotReloadCallbackReceiver.CompilationFinished += _ => Schedule(() => { calculateProfile(currentUser); });
        }

        private void calculateProfile(string username)
        {
            if (string.IsNullOrEmpty(username))
            {
                usernameTextBox.FlashColour(Color4.Red, 1);
                return;
            }

            calculationCancellatonToken?.Cancel();
            calculationCancellatonToken?.Dispose();

            loadingLayer.Show();
            calculationButton.State.Value = ButtonState.Loading;

            scores.Clear();

            calculationCancellatonToken = new CancellationTokenSource();
            var token = calculationCancellatonToken.Token;

            long milliStart = 0;
            
            Task.Run(async () =>
            {
                Schedule(() => loadingLayer.Text.Value = "Getting user data...");

                var player = await apiManager.GetJsonFromApi<APIUser>($"users/{username}/{ruleset.Value.ShortName}");

                currentUser = player.Username;

                Schedule(() =>
                {
                    if (userPanel != null)
                        userPanelContainer.Remove(userPanel, true);

                    userPanelContainer.Add(userPanel = new UserCard(player)
                    {
                        RelativeSizeAxes = Axes.X
                    });

                    layout.RowDimensions = new[] { new Dimension(GridSizeMode.Absolute, username_container_height), new Dimension(GridSizeMode.AutoSize), new Dimension() };
                });

                if (token.IsCancellationRequested)
                    return;

                var plays = new List<ExtendedScore>();

                var rulesetInstance = ruleset.Value.CreateInstance();

                Schedule(() => loadingLayer.Text.Value = $"Calculating {player.Username} top scores...");

                string osuPath = configManager.GetBindable<string>(Settings.OsuFolderPath).Value;
                SortedDictionary<string, DbBeatmap> beatmapDict = dbMapper(osuPath);
                var scoresDatabase = DatabaseDecoder.DecodeScores(new FileStream(osuPath + @"\scores.db", FileMode.Open));
                int uniqueScoresCount = 0;

                milliStart = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond; //timer start

                foreach (var scoreList in scoresDatabase.Scores)
                {
                    if (token.IsCancellationRequested)
                        return;
                    Schedule(() => loadingLayer.Text.Value = $"Calculating {scoreList.Item2[0].BeatmapMD5Hash}");

                    (DbBeatmap, bool) locallookup = localBeatmapLookup(scoreList.Item2[0], beatmapDict);
                    ProcessorWorkingBeatmap working = null;
                    var tempScores = new List<ExtendedScore>();
                    if(!locallookup.Item2 || locallookup.Item1.RankedStatus != RankedStatus.Ranked)
                        continue; //if map doesnt exist in db or if it is not a ranked diff then skip;

                    string[] paths = { configManager.GetBindable<string>(Settings.OsuFolderPath).Value, "Songs", locallookup.Item1.FolderName, locallookup.Item1.FileName };
                    string beatmapFilePath = Path.Combine(paths);
                    if (File.Exists(beatmapFilePath)) //song can exist in db but the corresponding file might not 
                        working = ProcessorWorkingBeatmap.FromFileOrId(beatmapFilePath, cachePath: configManager.GetBindable<string>(Settings.CachePath).Value);

                    //keep a count of ranked diffs with a score on it for bonusPP
                    uniqueScoresCount++;

                    var difficultyCalculator = rulesetInstance.CreateDifficultyCalculator(working);
                    var performanceCalculator = rulesetInstance.CreatePerformanceCalculator();
                    List<ParsedScore> sortedScores = scoreList.Item2.OrderBy(x => x.Mods).ToList();
                    DifficultyAttributes difficultyAttributes = null;
                    LegacyMods prevMods = (LegacyMods)sortedScores[0].Mods;
                    
                    foreach(var decodedScore in sortedScores)
                    {
                        if(decodedScore.ScoreId == 0) { continue; } //only calculate PB's on a diff
                        if ((int)decodedScore.Ruleset != ruleset.Value.OnlineID) { continue; } //only calculate scores from selected ruleset
                        if (!(player.PreviousUsernames.Contains(decodedScore.PlayerName) || player.Username.Equals(decodedScore.PlayerName))) { continue; } //only calculate scores set by name inputted
                        
                        var soloScore = populateSoloScoreInfo(decodedScore, working, rulesetInstance);

                        Mod[] mods = soloScore.Mods.Select(x => x.ToMod(rulesetInstance)).ToArray();

                        var scoreInfo = soloScore.ToScoreInfo(rulesets, working.BeatmapInfo);

                        //Reuse diff attr. when mods haven't changed
                        if ((LegacyMods)decodedScore.Mods != prevMods || difficultyAttributes == null) 
                        { 
                            difficultyAttributes = difficultyCalculator.Calculate(RulesetHelper.ConvertToLegacyDifficultyAdjustmentMods(rulesetInstance, mods));
                            prevMods = (LegacyMods)decodedScore.Mods;
                        }

                        var livePp = soloScore.PP ?? 0.0;
                        var perfAttributes = performanceCalculator?.Calculate(scoreInfo, difficultyAttributes);
                        soloScore.PP = perfAttributes?.Total ?? 0.0;

                        var extendedScore = new ExtendedScore(soloScore, livePp, perfAttributes);
                        tempScores.Add(extendedScore);
                    }

                    if (tempScores.Count == 0)
                        continue;

                    var topScore = tempScores.OrderByDescending(x => x.SoloScore.PP).First();                    
                    plays.Add(topScore);
                    Schedule(() => scores.Add(new ExtendedProfileScore(topScore)));
                }
                
                var milliEnd = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond; //timer end
                var totalTime = TimeSpan.FromMilliseconds(milliEnd - milliStart).TotalSeconds;

                Console.WriteLine($"Finish {totalTime}s");

                if (token.IsCancellationRequested)
                    return;

                var localOrdered = plays.OrderByDescending(x => x.SoloScore.PP).ToList();
                var liveOrdered = plays.OrderByDescending(x => x.LivePP).ToList();

                Schedule(() =>
                {
                    foreach (var play in plays)
                    {
                        play.Position.Value = localOrdered.IndexOf(play) + 1;
                        play.PositionChange.Value = liveOrdered.IndexOf(play) - localOrdered.IndexOf(play);
                        scores.SetLayoutPosition(scores[liveOrdered.IndexOf(play)], localOrdered.IndexOf(play));
                    }
                });

                decimal totalLocalPP = 0;
                for (var i = 0; i < localOrdered.Count; i++)
                    totalLocalPP += (decimal)(Math.Pow(0.95, i) * (localOrdered[i].SoloScore.PP ?? 0));

                decimal totalLivePP = player.Statistics.PP ?? (decimal)0.0;

                decimal nonBonusLivePP = 0;
                for (var i = 0; i < liveOrdered.Count; i++)
                    nonBonusLivePP += (decimal)(Math.Pow(0.95, i) * liveOrdered[i].LivePP);

                //Calculate bonusPP based of unique score count on ranked diffs
                var playcountBonusPP = (decimal)(416.6667 * (1 - Math.Pow(0.9994, uniqueScoresCount)));
                totalLocalPP += playcountBonusPP;

                Schedule(() =>
                {
                    userPanel.Data.Value = new UserCardData
                    {
                        LivePP = totalLivePP,
                        LocalPP = totalLocalPP,
                        PlaycountPP = playcountBonusPP
                    };
                });
            }, token).ContinueWith(t =>
            {
                Logger.Log(t.Exception?.ToString(), level: LogLevel.Error);
                notificationDisplay.Display(new Notification(t.Exception?.Flatten().Message));
            }, TaskContinuationOptions.OnlyOnFaulted).ContinueWith(t =>
            {
                Schedule(() =>
                {
                    loadingLayer.Hide();
                    calculationButton.State.Value = ButtonState.Done;
                });
            }, token);
        }

        protected override void Dispose(bool isDisposing)
        {
            base.Dispose(isDisposing);

            calculationCancellatonToken?.Cancel();
            calculationCancellatonToken?.Dispose();
            calculationCancellatonToken = null;
        }
        private static SortedDictionary<string, DbBeatmap> dbMapper(string osuPath)
        {
            //decode db and return list of DbBeatmaps => map to dictionary with beatmap md5Hash as key
            var beatmapDict = DatabaseDecoder.DecodeOsu(new FileStream(osuPath + @"\osu!.db", FileMode.Open)).Beatmaps.ToDictionary(x => x.MD5Hash ?? "", x => x);
            SortedDictionary<string, DbBeatmap> sortedBeatmapDict = new SortedDictionary<string, DbBeatmap>(beatmapDict);
            return sortedBeatmapDict;
        }
        private static (DbBeatmap, bool) localBeatmapLookup(ParsedScore score, SortedDictionary<string, DbBeatmap> beatmaps)
        {
            if (beatmaps.ContainsKey(score.BeatmapMD5Hash))
            {
                return (beatmaps[score.BeatmapMD5Hash], true);
            }

            return (null, false);
        }
        private static SoloScoreInfo populateSoloScoreInfo(ParsedScore score, ProcessorWorkingBeatmap workingBeatmap, Ruleset ruleset)
        {
            var dummyMods = ruleset.ConvertFromLegacyMods((LegacyMods)score.Mods).ToArray();
            APIMod[] apimods = dummyMods.Select(m => new APIMod(m)).ToArray();
            APIBeatmapSet dummySet = new APIBeatmapSet
            {
                Title = workingBeatmap.Metadata.Title,
                Artist = workingBeatmap.Metadata.Artist,
            };
            APIBeatmap dummyBeatmap = new APIBeatmap
            {
                OnlineID = workingBeatmap.BeatmapInfo.OnlineID,
                DifficultyName = workingBeatmap.BeatmapInfo.DifficultyName,
            };
            Dictionary<HitResult, int> dummyStatistics = new Dictionary<HitResult, int>(){
                { HitResult.Great, score.Count300},
                { HitResult.Miss, score.CountMiss},
                { HitResult.Ok, score.Count100},
                { HitResult.Meh, score.Count50},
            };
            SoloScoreInfo soloScoreInfo = new SoloScoreInfo
            {
                Accuracy = RulesetHelper.GetAccuracyForRuleset(ruleset.RulesetInfo, dummyStatistics),
                Statistics = dummyStatistics,
                MaxCombo = score.Combo,
                Mods = apimods, 
                Beatmap = dummyBeatmap,
                EndedAt = score.ScoreTimestamp,
                BeatmapSet = dummySet,
            };
            soloScoreInfo.Rank = new ScoreProcessor(ruleset).RankFromAccuracy(soloScoreInfo.Accuracy);
      
            return soloScoreInfo;
        }
    }
    
}
