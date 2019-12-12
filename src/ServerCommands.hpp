/* Copyright (C) 2019  Wolfgang E. Sanyer <ezzieyguywuf@gmail.com>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If
 * a copy of the MPL was not distributed with this file, You can obtain one at
 * https://mozilla.org/MPL/2.0/.
 */

#ifndef MYCAD_SERVER_COMMAND
#define MYCAD_SERVER_COMMAND

namespace MyCAD
{
/** @brief Contains all the Commands that Server knows how to execute */
namespace Commands
{
/** @brief A Command is an action that Server knows how to perform */
class Command
{
    public:
        /** @brief token CANNOT contain a space 
         *  @param token a string which represents this Command
         *  @param help  documentation for the Command.
         */
        Command(std::string token, std::string help="");
        virtual ~Command()=0;

        /** @brief returns the identifying token for this Command */
        std::string const& token() const;

        /** @brief returns the help documentation */
        void getHelp() const;

        /** @brief The good stuff!!! 
         *  @param data All the information the user passed after the token.
         */
        virtual void execute(std::string const& data) const;

    private:
        std::string myToken;
        std::string myHelp;
};

/** @brief Used to register commands that Server understands */
void RegisterCommand(std::unique_ptr<Command> command);
/** @brief This can be called to register all the commands at runtime */
void RegisterAllCommands();

/** @name Commands
 *  @{
 *  These are the commands that Server will recognize (if they get registered)
 */
class Version : public Command
{
    public:
        Version();
        ~Version() override = default;

        void execute(std::string const& data) const override;
};
class Add : public Command
{
    public:
        Add();
        ~Add() override = default;

        void execute(std::string const& data) const override;
};
/// @}
} // namespace Commands
} // namespace MyCAD

#endif // MYCAD_SERVER_COMMAND
